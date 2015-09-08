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

type generation_issue =
| Template_error of string
| Doc_error of CodocAnalysis.doc_error

type unit_issue =
| Module_resolution_failed of string
| Xml_error of (string * Xmlm.pos * string)

type generated_sub = {
  html_file : string option;
  issues    : generation_issue list;
}

type generated_unit = {
  name        : string;
  root        : CodocDoc.root;
  xml_file    : string;
  substructs  : generated_sub CodocUnit.Substruct.name;
  unit_issues : unit_issue list;
}

type pkg = {
  pkg_name : string;
  index    : string; (* TODO: should have xml and html like unit? *)
}

type t = {
  pkgs  : pkg StringMap.t;
  units : generated_unit StringMap.t;
  root  : string;
  path  : string;
  cache : (string, t list) Hashtbl.t;
}

let xmlns = CodocXml.index_ns

let (/) = Filename.concat

let list_of_map map = List.rev_map snd (StringMap.bindings map)

let error_of_issue path = function
  | Template_error message ->
    `Error (false, Printf.sprintf "%s: Template error:\n%s" path message)
  | Doc_error doc_error ->
    let message = CodocAnalysis.string_of_error doc_error in
    `Error (false, Printf.sprintf "%s: Documentation error:\n%s" path message)

let error_of_unit_issue path = function
  | Module_resolution_failed mod_name ->
    `Error (false, "writing "^path^" resolution of "^mod_name^" failed")
  | Xml_error (path, (l,c), s) ->
    `Error (false, Printf.sprintf "%s:%d:%d: XML error %s" path l c s)

let index_filename = "index.xml"

let index_file ?(rel_index=index_filename) dir = Filename.concat dir rel_index

let xml_of_generation_issue = function
  | Template_error message ->
    [`El ((("","template-error"),[]),[`Data message])]
  | Doc_error doc_error ->
    [`El ((("","doc-error"),[]),CodocAnalysis.xml_of_error doc_error)]

let xml_of_unit_issue = function
  | Module_resolution_failed mod_name ->
    let attrs = [
      ("","module"),mod_name;
    ] in
    [`El ((("","resolution-failed"),attrs),[])]
  | Xml_error (xml_file, (l,c), msg) ->
    let attrs = [
      ("","href"),xml_file;
      ("","line"),string_of_int l;
      ("","col"),string_of_int c;
    ] in
    [`El ((("","xml-error"),attrs),[`Data msg])]

let xml_of_substruct (acc,u) typ name children =
  let issues = match u.issues with
    | [] -> []
    | issues ->
      [`El ((("","issues"),[]),
            List.(flatten (map xml_of_generation_issue issues)))]
  in
  let html_file = match u.html_file with
    | None -> []
    | Some html_file ->
      [`El ((("","file"),[("","type"),"text/html";("","href"),html_file]),[])]
  in
  (`El ((("",typ),[("","name"),name]),html_file@children@issues))::acc

let rec fold_xml_of_substruct acc = CodocUnit.Substruct.(function
  | ClassName (c, u)::rest ->
    let a = xml_of_substruct (acc,u) "class" c [] in
    fold_xml_of_substruct a rest
  | ClassTypeName (c, u)::rest ->
    let a = xml_of_substruct (acc,u) "classtype" c [] in
    fold_xml_of_substruct a rest
  | ModuleName (m, l, u)::rest ->
    let a = xml_of_substruct (acc,u) "module" m (fold_xml_of_substruct [] l) in
    fold_xml_of_substruct a rest
  | ModuleTypeName (m, l, u)::rest ->
    let a =
      xml_of_substruct (acc,u) "moduletype" m (fold_xml_of_substruct [] l)
    in
    fold_xml_of_substruct a rest
  | UnitName (m, l, u)::rest ->
    let a = xml_of_substruct (acc,u) "module" m (fold_xml_of_substruct [] l) in
    fold_xml_of_substruct a rest
  | [] -> acc
)

let xml_of_generated_unit ({ name; root; xml_file; substructs; unit_issues }) =
  let issues = match unit_issues with
    | [] -> []
    | issues ->
      [`El ((("","issues"),[]),
            List.(flatten (map xml_of_unit_issue issues)))]
  in
  let substructs = fold_xml_of_substruct [] [substructs] in
  [`El ((("","unit"),[("","name"),name]),
        (`El ((("","file"),
               [("","type"),"application/xml";("","href"),xml_file]),[]);
        )::
        (`El ((("","root"),[]),
              CodocDoc.xml_of_root xmlns root)
        )::(substructs@issues)
       )
  ]

let xml_of_pkg ({ pkg_name; index }) =
  let attrs = [
    ("","name"),pkg_name;
    ("","href"),index;
  ] in
  [`El ((("","package"),attrs),[])]

let to_xml ({ units; pkgs }) =
  [`El ((("","doc-index"),[("","xmlns"),xmlns]),
        List.flatten (
          (list_of_map (StringMap.map xml_of_pkg pkgs))@
          (list_of_map (StringMap.map xml_of_generated_unit units)))
       )
  ]

let eat = CodocXml.eat
let must_end = CodocXml.must_end
let just_data = CodocXml.just_data

let rec generation_issue_of_xml xml = match Xmlm.peek xml with
  | `El_start ((ns,"doc-error"),[]) when ns = xmlns ->
    eat xml;
    let doc_error = CodocAnalysis.error_of_xml xml in
    must_end xml;
    Doc_error doc_error
  | `El_start ((ns,"template-error"),[]) ->
    eat xml;
    let message = just_data xml in
    must_end xml;
    Template_error message
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element @ gen issue"
  | `El_end -> (* TODO: fixme *) failwith "unexpected end @ gen issue"
  | `Data _ | `Dtd _ -> eat xml; generation_issue_of_xml xml

let rec unit_issue_of_xml xml = match Xmlm.peek xml with
  | `El_start ((ns,"resolution-failed"),[("","module"),name]) when ns = xmlns ->
    eat xml;
    must_end xml;
    Module_resolution_failed name
  | `El_start ((ns,"xml-error"),
               [("","href"),href; ("","line"),line; ("","col"), col])
      when ns = xmlns ->
    (* TODO: permutation of attrs, bad ints *)
    eat xml;
    let message = just_data xml in
    must_end xml;
    Xml_error (href, (int_of_string line,int_of_string col), message)
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element @ unit issue"
  | `El_end -> (* TODO: fixme *) failwith "unexpected end @ unit issue"
  | `Data _ | `Dtd _ -> eat xml; unit_issue_of_xml xml

let rec issues_of_xml xml issues = match Xmlm.peek xml with
  | `El_start _ ->
    let issue = generation_issue_of_xml xml in
    issues_of_xml xml (issue::issues)
  | `El_end -> eat xml; issues
  | `Dtd _ | `Data _ -> eat xml; issues_of_xml xml issues

let rec unit_issues_of_xml xml issues = match Xmlm.peek xml with
  | `El_start _ ->
    let issue = unit_issue_of_xml xml in
    unit_issues_of_xml xml (issue::issues)
  | `El_end -> eat xml; issues
  | `Dtd _ | `Data _ -> eat xml; unit_issues_of_xml xml issues

let rec inside xml tag fn = match Xmlm.peek xml with
  | `El_start (el,attrs) when el = tag ->
    eat xml;
    fn attrs
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element @ inside"
  | `El_end -> []
  | `Data _ | `Dtd _ -> eat xml; inside xml tag fn

let rec files_of_xml xml files = match Xmlm.peek xml with
  | `El_start ((ns,"file"),[("","type"),typ;("","href"),href])
  | `El_start ((ns,"file"),[("","href"),href;("","type"),typ]) when ns = xmlns ->
    eat xml;
    must_end xml;
    files_of_xml xml ((typ,href)::files)
  | `El_start _ | `El_end -> files
  | `Dtd _ | `Data _ -> eat xml; files_of_xml xml files

let rec substructs_of_xml xml subs = CodocUnit.Substruct.(
  match Xmlm.peek xml with
  | `El_start ((ns,"class"),[("","name"),name]) when ns = xmlns ->
    eat xml;
    let _, sub = substruct_body_of_xml xml in
    must_end xml;
    substructs_of_xml xml (ClassName (name,sub)::subs)
  | `El_start ((ns,"classtype"),[("","name"),name]) when ns = xmlns ->
    eat xml;
    let _, sub = substruct_body_of_xml xml in
    must_end xml;
    substructs_of_xml xml (ClassTypeName (name,sub)::subs)
  | `El_start ((ns,"module"),[("","name"),name]) when ns = xmlns ->
    eat xml;
    let children, sub = substruct_body_of_xml xml in
    must_end xml;
    substructs_of_xml xml (ModuleName (name,children,sub)::subs)
  | `El_start ((ns,"moduletype"),[("","name"),name]) when ns = xmlns ->
    eat xml;
    let children, sub = substruct_body_of_xml xml in
    must_end xml;
    substructs_of_xml xml (ModuleTypeName (name,children,sub)::subs)
  | `El_start _ | `El_end -> subs
  | `Dtd _ | `Data _ -> eat xml; substructs_of_xml xml subs
)
and substruct_body_of_xml xml =
  let files = files_of_xml xml [] in
  let html_file =
    try Some (List.assoc "text/html" files)
    with Not_found -> None
  in
  let children = substructs_of_xml xml [] in
  let issues = inside xml (xmlns,"issues") (fun _ -> issues_of_xml xml []) in
  (children, { html_file; issues })

let generated_unit_of_xml xml name =
  let files = files_of_xml xml [] in
  let xml_file = List.assoc "application/xml" files in
  let root =
    let rec loop xml =
      match Xmlm.peek xml with
      | `El_start ((ns,"root"),_) when ns = xmlns ->
        eat xml;
        let res =
          Xmlm.input_tree ~el:(CodocDoc.root_of_xml xmlns)
            ~data:CodocDoc.data_of_xml xml
        in
        must_end xml;
        res
      | `El_start _ -> (* TODO: fixme *) failwith "missing root data"
      | `El_end -> None
      | `Data _ | `Dtd _ -> eat xml; loop xml
    in
    match loop xml with
    | None -> failwith "missing root data"
    | Some root -> root
  in
  match substructs_of_xml xml [] with
  | [] ->
    must_end xml;
    failwith "not enough structs in unit" (* TODO: fixme *)
  | _::_::_ ->
    must_end xml;
    failwith "too many structs in unit" (* TODO: fixme *)
  | [substructs] ->
    let unit_issues =
      inside xml (xmlns,"issues") (fun _ -> unit_issues_of_xml xml [])
    in
    must_end xml;
    { name; root; xml_file; unit_issues; substructs; }

let pkg_of_xml xml pkg_name index =
  must_end xml;
  { pkg_name; index }

let empty root path = {
  units = StringMap.empty;
  pkgs = StringMap.empty;
  root;
  path;
  cache = Hashtbl.create 10;
}

let rec of_xml root path xml =
  let rec doc_index index = match Xmlm.input xml with
      | `El_start ((ns,"unit"),[("","name"), name]) when ns = xmlns ->
        doc_index { index with
          units = StringMap.add name (generated_unit_of_xml xml name)
            index.units
        }
      | `El_start ((ns,"package"),[("","name"), name; ("","href"), href])
      | `El_start ((ns,"package"),[("","href"), href; ("","name"), name])
          when ns = xmlns ->
        doc_index { index with
          pkgs = StringMap.add name (pkg_of_xml xml name href) index.pkgs
        }
      | `El_start _ -> (* TODO: fixme *) failwith "unknown element @ index"
      | `El_end -> index
      | `Data _ | `Dtd _ -> doc_index index
  in
  let start = function
    | ((ns,"doc-index"),_) when ns = xmlns -> doc_index (empty root path)
    | _ -> (* TODO: fixme *) failwith "unknown root node"
  in
  match Xmlm.input xml with
  | `El_start tag -> start tag
  | `El_end -> empty root path
  | `Data _ | `Dtd _ -> of_xml root path xml

let set_gunit index gunit =
  { index with units = StringMap.add gunit.name gunit index.units }

let add_packages from into =
  let pkgs = StringMap.merge (fun _k l r ->
    match l, r with
    | None, None -> None
    | Some x, None | None, Some x -> Some x
    | Some _, Some y -> Some y
  ) from.pkgs into.pkgs in
  { into with pkgs }

let read root path =
  let index_path = root / path in
  if Sys.file_exists index_path
  then
    let ic = open_in index_path in
    let input = Xmlm.make_input (`Channel ic) in
    let index = of_xml root path input in
    let () = close_in ic in
    index
  else empty root path

let read_cache ({ cache; root }) path =
  let root_path = root / path in
  try List.hd (Hashtbl.find cache root_path)
  with Not_found ->
    let idx = { (read root path) with cache } in
    Hashtbl.replace cache root_path [idx];
    idx

let read_cache_rel index rel_path =
  let dir = match Filename.dirname index.path with "." -> "" | p -> p in
  read_cache index (dir / rel_path)

let write index =
  let oc = open_out (index.root / index.path) in
  let output = Xmlm.make_output (`Channel oc) in
  Xmlm.output output (`Dtd None);
  Xmlm.output_tree (fun x -> x) output (List.hd (to_xml index));
  close_out oc

let write_cache ({ root; path; cache } as index) =
  let root_path = root / path in
  let previous = try Hashtbl.find cache root_path
    with Not_found -> [empty root path]
  in Hashtbl.replace cache root_path (index :: previous)

let flush_cache index = Hashtbl.iter (fun path -> function
  | [] | [_] -> ()
  | updated::_ -> write updated
) index.cache

(* TODO: check for fallback? *)
(* TODO: WARNING: only good for creation! *)
let traverse ?rel_index dir pkg =
  let index_file = index_file ?rel_index in
  let rec descend acc rel path = match acc, path with
    | _, [] -> acc
    | [], path ->
      let dir = dir / rel in
      descend [rel, read dir (index_file "")] rel path
    | (parent_path,parent_index)::ancs, sub::more ->
      let pkg =
        try StringMap.find sub parent_index.pkgs
        with Not_found -> { pkg_name = sub; index = index_file sub }
      in
      let index = read dir (rel / pkg.index) in
      let rel = rel / sub in
      let parent_index = {
        parent_index with pkgs = StringMap.add sub pkg parent_index.pkgs
      } in
      let acc = (rel,index)::(parent_path,parent_index)::ancs in
      descend acc rel more
  in
  match descend [] "" (Stringext.split ~on:'/' pkg) with
  | [] -> ("", read dir (index_file "")), []
  | pkg_index::parents -> pkg_index, parents

let goto index pkg =
  let rec descend index = function
    | [] -> index
    | step::rest ->
      let subpkg = StringMap.find step index.pkgs in
      descend (read_cache_rel index subpkg.index) rest
  in
  descend index (Stringext.split ~on:'/' pkg)

let fold_down ~unit_f ~pkg_f acc index =
  let rec descend index acc =
    let acc = StringMap.fold (fun _name gunit acc ->
      unit_f acc index gunit
    ) index.units acc in
    StringMap.fold (fun _name pkg acc ->
      let index = read_cache_rel index pkg.index in
      pkg_f (descend index) acc index
    ) index.pkgs acc
  in
  pkg_f (descend index) acc index

let fold_down_units = fold_down ~pkg_f:(fun rc acc _index -> rc acc)

(* TODO: this is a very specific merge function... describe *)
(*let merge from into =
  if from.cache != into.cache
  then (* TODO: fixme! *)
    failwith "can't merge indexes from different cache domains"
  else
    let units = StringMap.merge (fun _k l r ->
      match l, r with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some { xml_file; issues }, Some y -> Some { y with xml_file; issues }
    ) from.units into.units in
    let pkgs = StringMap.merge (fun _k l r ->
      match l, r with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some _, Some y -> Some y
    ) from.pkgs into.pkgs in
    { into with units; pkgs; }
*)
