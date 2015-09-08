(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module StringMap = Map.Make(String)
module RootTable = Hashtbl.Make(CodocDoc.Root)

open CodocDoc

(* TODO: use digest, too *)
type t = {
  root_by_name : (string, root) Hashtbl.t;
  unit_by_root : root DocOck.Types.Unit.t Lazy.t RootTable.t;
  path_by_root : string RootTable.t;
  failures : string RootTable.t;
  resolver : root DocOck.resolver;
  expander : root DocOck.expander;
}

let (/) = Filename.concat

let xml_error xml_file ?start (line,col) s = match start with
  | Some (start_line, start_col) ->
    Printf.eprintf "\n%s line %d column %d - line %d column %d:\n%s\n\n"
      xml_file start_line start_col line col s
  | None ->
    Printf.eprintf "\n%s line %d column %d:\n%s\n\n" xml_file line col s

let path_by_root env root =
  try RootTable.find env.path_by_root root
  with Not_found ->
    let cmti_path = Root.(to_path (to_source root)) in
    let name = Maps.name_of_root root in
    failwith ("couldn't find path for root "^name^"@"^cmti_path) (* TODO *)

let index env path name root unit =
  Hashtbl.add env.root_by_name name root;
  RootTable.replace env.unit_by_root root (Lazy.from_val unit);
  RootTable.replace env.path_by_root root path

let rec index_units env index = CodocIndex.(
  fold_down_units
    ~unit_f:(fun env index { name; root; xml_file } ->
      Hashtbl.add env.root_by_name name root;
      let dir = Filename.dirname index.path in
      RootTable.replace env.path_by_root root (dir / xml_file);
      let unit =
        lazy (
          let path_root = index.root / dir in
          let xml_root = path_root / xml_file in
          let ic = open_in xml_root in
          let input = Xmlm.make_input (`Channel ic) in
          match DocOckXmlParse.file CodocXml.doc_parser input with
          | DocOckXmlParse.Error (start, pos, s) ->
            close_in ic;
            (* TODO: fixme? different/better error style? *)
            xml_error xml_root ?start pos s;
            exit 1
          | DocOckXmlParse.Ok unit ->
            close_in ic;
            unit
        )
      in
      RootTable.replace env.unit_by_root root unit;
      env
    )
    env index
)

let find_path path_by_root root =
  try
    RootTable.find path_by_root root
  with
  | Not_found ->
    let cmti_path = Root.(to_path (to_source root)) in
    let name = Maps.name_of_root root in
    failwith ("couldn't find path for root "^name^"@"^cmti_path) (* TODO *)

(* TODO: Recursively all the way down? *)
let relativize_root path_by_root from_path root =
  let open CodocUtil in
  let to_path = Filename.dirname (find_path path_by_root root) in
  let rec relativize = function
    | Xml (path, parent) ->
      Xml (rel_of_path (depth from_path) to_path / path, relativize parent)
    | Cm cm -> Cm cm
    | Resolved ({ resolution_root }, parent) ->
      let path = rel_of_path (depth from_path) to_path in
      let resolution_root = path / resolution_root in
      Resolved ({ resolution_root }, relativize parent)
    | Proj (spath, parent) -> Proj (spath, relativize parent)
  in
  relativize root

class map_roots f = object
  method root x = f x
  inherit [CodocDoc.root] DocOckMaps.paths
  inherit [CodocDoc.root] DocOckMaps.types
end

let relativize_unit_roots path_by_root from_path unit =
  let map = new map_roots (relativize_root path_by_root from_path) in
  map#unit unit

let find_import roots unit name =
  let open DocOck.Types.Unit in
  let open DocOck.Types.Unit.Import in
  let rec loop = function
    | [] -> raise Not_found
    | imp :: rest ->
      match imp with
      | Unresolved(name', Some digest) ->
          if name = name' then
            List.find
              (fun root ->
                 digest = CodocDoc.Root.to_digest root)
              roots
          else loop rest
      | Unresolved(name', None) ->
          if name = name' then raise Not_found
          else loop rest
      | Resolved root ->
          try
            List.find
              (fun root' -> CodocDoc.Root.equal root root')
              roots
          with Not_found -> loop rest
  in
  loop unit.imports

let create doc_index =
  let equal = CodocDoc.Root.equal in
  let hash = CodocDoc.Root.hash in
  let root_by_name = Hashtbl.create 10 in
  let unit_by_root = RootTable.create 10 in
  let path_by_root = RootTable.create 10 in
  let failures = RootTable.create 10 in
  let lookup req_unit mod_name =
    let req_root, req_name = CodocUtil.root_of_unit req_unit in
    (* TODO: this can raise if file-loaded roots aren't indexed... *)
    let req_path = find_path path_by_root req_root in
    let candidates = Hashtbl.find_all root_by_name mod_name in
    match find_import candidates req_unit mod_name with
    | root -> Some (relativize_root path_by_root req_path root)
    | exception Not_found ->
      match candidates with
      | root :: _ -> Some (relativize_root path_by_root req_path root)
      | [] ->
        let fails = RootTable.find_all failures req_root in
        if List.mem mod_name fails then None
        else (RootTable.add failures req_root mod_name; None)
  in
  let resolve_fetch root =
    match RootTable.find unit_by_root root with
    | exception Not_found ->
      let name = CodocDoc.Root.to_name root in
      failwith ("No unit for root " ^ name)
    | lazy unit -> unit
  in
  let expand_fetch ~root root' =
    match RootTable.find unit_by_root root' with
    | exception Not_found ->
      let name = CodocDoc.Root.to_name root' in
      failwith ("No unit for root " ^ name)
    | lazy unit ->
        let path = find_path path_by_root root in
        relativize_unit_roots path_by_root path unit
  in
  let resolver =
    DocOck.build_resolver ~equal ~hash lookup resolve_fetch
  in
  let expander =
    DocOck.build_expander ~equal ~hash expand_fetch
  in
  let env =
    { root_by_name; unit_by_root; path_by_root;
      failures; resolver; expander; }
  in
  index_units env doc_index

let create_for_unit unit =
  let self_root, self_name = CodocUtil.root_of_unit unit in
  let self_path = CodocDoc.Root.to_path self_root in
  let equal = CodocDoc.Root.equal in
  let hash = CodocDoc.Root.hash in
  let root_by_name = Hashtbl.create 1 in
  let unit_by_root = RootTable.create 1 in
  let path_by_root = RootTable.create 1 in
  let failures = RootTable.create 10 in
  let lookup req_unit mod_name =
    let req_root, req_name = CodocUtil.root_of_unit req_unit in
    if equal self_root req_root && self_name = mod_name then
      Some self_root
    else begin
      let fails = RootTable.find_all failures req_root in
      if List.mem mod_name fails then None
      else (RootTable.add failures req_root mod_name; None)
    end
  in
  let resolve_fetch root =
    match RootTable.find unit_by_root root with
    | exception Not_found ->
      let name = CodocDoc.Root.to_name root in
      failwith ("No unit for root " ^ name)
    | lazy unit -> unit
  in
  let expand_fetch ~root root' =
    match RootTable.find unit_by_root root' with
    | exception Not_found ->
      let name = CodocDoc.Root.to_name root' in
      failwith ("No unit for root " ^ name)
    | lazy unit ->
        let path = find_path path_by_root root in
        relativize_unit_roots path_by_root path unit
  in
  let resolver =
    DocOck.build_resolver ~equal ~hash lookup resolve_fetch
  in
  let expander =
    DocOck.build_expander ~equal ~hash expand_fetch
  in
  let env =
    { root_by_name; unit_by_root; path_by_root;
      failures; resolver; expander; }
  in
  index env self_path self_name self_root unit;
  env


let root_by_name env name =
  match Hashtbl.find env.root_by_name name with
  | root -> Some root
  | exception Not_found -> None

let unit_by_root env root =
  match RootTable.find env.unit_by_root root with
  | lazy unit -> Some unit
  | exception Not_found -> None

let unit_by_name env name =
  match root_by_name env name with
  | None -> None
  | Some root -> unit_by_root env root

let path_by_root env root =
  find_path env.path_by_root root

let resolver env = env.resolver

let expander env = env.expander

let failures_of_root env root = RootTable.find_all env.failures root

