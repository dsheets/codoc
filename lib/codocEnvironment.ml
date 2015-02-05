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
  root_by_name : (string, root Lazy.t) Hashtbl.t;
  unit_by_root : root DocOckTypes.Unit.t RootTable.t;
  path_by_root : (root, string) Hashtbl.t;
  failures     : (root, string) Hashtbl.t;
  resolver     : root DocOck.resolver;
}

let (/) = Filename.concat

let xml_error xml_file ?start (line,col) s = match start with
  | Some (start_line, start_col) ->
    Printf.eprintf "\n%s line %d column %d - line %d column %d:\n%s\n\n"
      xml_file start_line start_col line col s
  | None ->
    Printf.eprintf "\n%s line %d column %d:\n%s\n\n" xml_file line col s

let root_by_name env name =
  try Some (Lazy.force (Hashtbl.find env.root_by_name name))
  with Not_found -> None

let unit_by_root env root =
  let name = CodocDoc.Maps.name_of_root root in
  try RootTable.find env.unit_by_root root
  with Not_found ->
    (*match root_by_name env name with
    | None ->*)
      (* TODO: most likely caused by failure to index *)
      failwith ("couldn't find unit for root or root by name "^name) (* TODO *)
    (*| Some found_root ->
      if found_root = root
      then try Hashtbl.find env.unit_by_root root
        with Not_found ->
          let cmti_path = CodocDoc.Root.(to_path (to_source root)) in
          (* TODO *)
          failwith ("couldn't find unit for root "^name^"@"^cmti_path)
      else failwith ("unit_by_root: found_root <> root") (* TODO *)*)

let unit_by_name env name = match root_by_name env name with
  | None -> failwith ("couldn't find unit for name "^name) (* TODO *)
  | Some root -> unit_by_root env root

let path_by_root env root =
  try Hashtbl.find env.path_by_root root
  with Not_found ->
    let cmti_path = Root.(to_path (to_source root)) in
    let name = Maps.name_of_root root in
    failwith ("couldn't find path for root "^name^"@"^cmti_path) (* TODO *)

let index env path name root unit =
  Hashtbl.replace env.root_by_name name (Lazy.from_val root);
  RootTable.replace env.unit_by_root root unit;
  Hashtbl.replace env.path_by_root root path

let rec index_units env index = CodocIndex.(
  fold_down_units
    ~unit_f:(fun env index { mod_name; xml_file } ->
      Hashtbl.replace env.root_by_name mod_name
        (Lazy.from_fun (fun () ->
          let dir = Filename.dirname index.path in
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
            let root, mod_name = CodocUtil.root_of_unit unit in
            RootTable.replace env.unit_by_root root unit;
            Hashtbl.replace env.path_by_root root (dir / xml_file);
            root
         ));
      env
    )
    env index
)

(* TODO: Recursively all the way down? *)
let relativize_root env from_path root =
  let open CodocUtil in
  let to_path = Filename.dirname (path_by_root env root) in
  let rec relativize = function
    | Xml (path, parent) ->
      Xml (rel_of_path (depth from_path) to_path / path, relativize parent)
    | Html (path, parent) ->
      Html (rel_of_path (depth from_path) to_path / path, relativize parent)
    | Cmti cmti -> Cmti cmti
    | Resolved ({ resolution_root }, parent) ->
      let path = rel_of_path (depth from_path) to_path in
      let resolution_root = path / resolution_root in
      Resolved ({ resolution_root }, relativize parent)
    | Proj (spath, parent) -> Proj (spath, relativize parent)
  in
  relativize root

let create doc_index =
  let equal = CodocDoc.Root.equal in
  let hash = CodocDoc.Root.hash in
  let env_ref = ref None in
  let env = {
    root_by_name = Hashtbl.create 10;
    unit_by_root = RootTable.create 10;
    path_by_root = Hashtbl.create 10;
    failures = Hashtbl.create 10;
    resolver = DocOck.build_resolver ~equal ~hash
      (fun req_unit mod_name ->
        match !env_ref with None -> None | Some env ->
          let req_root, req_name = CodocUtil.root_of_unit req_unit in
          match root_by_name env mod_name with
          | Some root ->
            (* TODO: this can raise if file-loaded roots aren't indexed... *)
            let req_path = path_by_root env req_root in
            Some (relativize_root env req_path root)
          | None ->
            match Hashtbl.find_all env.failures req_root with
            | [] -> Hashtbl.add env.failures req_root mod_name; None
            | fails ->
              if List.mem mod_name fails then None
              else (Hashtbl.add env.failures req_root mod_name; None)
      )
      (fun root ->
        match !env_ref with None -> assert false | Some env ->
          unit_by_root env root);
  } in
  env_ref := Some env;
  index_units env doc_index

let resolver env = env.resolver

let failures_of_root env root = Hashtbl.find_all env.failures root
