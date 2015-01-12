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
| Module_resolution_failed of string
| Xml_error of string * string (* TODO: start/pos *)

type generated_unit = {
  mod_name  : string;
  xml_file  : string;
  html_file : string option;
  issues    : generation_issue list;
}

type pkg = {
  pkg_name : string;
  index    : string;
}

type t = {
  pkgs  : pkg StringMap.t;
  units : generated_unit StringMap.t;
}

let list_of_map map = List.rev_map snd (StringMap.bindings map)

let index_filename = "index.xml"

let index_file dir = Filename.concat dir index_filename

let xml_of_generation_issue = function
  | Module_resolution_failed mod_name ->
    <:xml<<resolution-failed module=$str:mod_name$/>&>>
  | Xml_error (xml_file, msg) ->
    (* TODO: start/pos *)
    <:xml<<xml-error href=$str:xml_file$>$str:msg$</xml-error>&>>

let xml_of_generated_unit ({ mod_name; xml_file; html_file; issues }) =
  let issues = match issues with
    | [] -> <:xml<&>>
    | issues -> <:xml<<issues>
      $list:List.map xml_of_generation_issue issues$
    </issues>&>>
  in
  let html_file = match html_file with
    | Some html_file -> <:xml<<file type="text/html" href=$str:html_file$/>&>>
    | None -> <:xml<&>>
  in
  <:xml<
  <unit name=$str:mod_name$>
    <file type="application/xml" href=$str:xml_file$/>
    $html_file$
    $issues$
  </unit>&>>

let xml_of_pkg ({ pkg_name; index }) =
  <:xml<
  <package name=$str:pkg_name$ href=$str:index$/>&>>

let to_xml ({ units; pkgs }) =
  <:xml<<doc-index>
  $list:list_of_map (StringMap.map xml_of_pkg pkgs)$
  $list:list_of_map (StringMap.map xml_of_generated_unit units)$
</doc-index>&>>

let string_of_pos (line,col) =
  Printf.sprintf "line %d, col %d" line col

let rec must_end xml = match Xmlm.input xml with
  | `El_end -> ()
  | `Data _ -> must_end xml (* TODO: ? *)
  | _ -> (* TODO: fixme *)
    failwith (Printf.sprintf "expected end: %s" (string_of_pos (Xmlm.pos xml)))

let eat xml = ignore (Xmlm.input xml)

let just_data xml = match Xmlm.peek xml with
  | `Data data -> eat xml; data
  | _ -> (* TODO: fixme *) failwith "expected data"

let rec generation_issue_of_xml xml = match Xmlm.peek xml with
  | `El_start (("","resolution-failed"),[("","module"),name]) ->
    eat xml;
    must_end xml;
    Module_resolution_failed name
  | `El_start (("","xml-error"),[("","href"),href]) ->
    eat xml;
    let message = just_data xml in
    must_end xml;
    Xml_error (href, message)
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
  | `El_end -> (* TODO: fixme *) failwith "unexpected end"
  | `Data _ | `Dtd _ -> eat xml; generation_issue_of_xml xml

let rec issues_of_xml xml issues = match Xmlm.peek xml with
  | `El_start _ ->
    let issue = generation_issue_of_xml xml in
    issues_of_xml xml (issue::issues)
  | `El_end -> eat xml; issues
  | `Dtd _ | `Data _ -> eat xml; issues_of_xml xml issues

let rec inside xml tag fn = match Xmlm.peek xml with
  | `El_start (el,attrs) when el = tag ->
    eat xml;
    fn attrs
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
  | `El_end -> []
  | `Data _ | `Dtd _ -> eat xml; inside xml tag fn

let rec files_of_xml xml files = match Xmlm.peek xml with
  | `El_start (("","file"),[("","type"),typ;("","href"),href])
  | `El_start (("","file"),[("","href"),href;("","type"),typ]) ->
    eat xml;
    must_end xml;
    files_of_xml xml ((typ,href)::files)
  | `El_start _ | `El_end -> files
  | `Dtd _ | `Data _ -> eat xml; files_of_xml xml files

let generated_unit_of_xml xml mod_name =
  let files = files_of_xml xml [] in
  let xml_file = List.assoc "application/xml" files in
  let html_file = Some (List.assoc "text/html" files) in
  let issues = inside xml ("","issues") (fun _ -> issues_of_xml xml []) in
  must_end xml;
  { mod_name; xml_file; html_file; issues }

let pkg_of_xml xml pkg_name index =
  must_end xml;
  { pkg_name; index }

let empty = { units = StringMap.empty; pkgs = StringMap.empty; }

let rec of_xml xml =
  let rec doc_index index = match Xmlm.input xml with
      | `El_start (("","unit"),[("","name"), name]) ->
        doc_index { index with
          units = StringMap.add name (generated_unit_of_xml xml name)
            index.units
        }
      | `El_start (("","package"),[("","name"), name; ("","href"), href])
      | `El_start (("","package"),[("","href"), href; ("","name"), name]) ->
        doc_index { index with
          pkgs = StringMap.add name (pkg_of_xml xml name href) index.pkgs
        }
      | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
      | `El_end -> index
      | `Data _ | `Dtd _ -> doc_index index
  in
  let start = function
    | (("","doc-index"),_) -> doc_index empty
    | _ -> (* TODO: fixme *) failwith "unknown root node"
  in
  match Xmlm.input xml with
  | `El_start tag -> start tag
  | `El_end -> empty
  | `Data _ | `Dtd _ -> of_xml xml

let read index_path =
  if Sys.file_exists index_path
  then
    let ic = open_in index_path in
    let input = Xmlm.make_input (`Channel ic) in
    let index = of_xml input in
    let () = close_in ic in
    index
  else empty

let write index_path index =
  let oc = open_out index_path in
  let output = Xmlm.make_output (`Channel oc) in
  Xmlm.output output (`Dtd None);
  Xmlm.output_tree (fun x -> x) output (List.hd (to_xml index));
  close_out oc

let traverse dir pkg =
  let rec descend acc rel path = match acc, path with
    | _, [] -> acc
    | [], path ->
      let dir = Filename.concat dir rel in
      descend [(rel, index_file rel), read (index_file dir)] rel path
    | (parent_paths,parent_index)::ancs, sub::more ->
      let rel = Filename.concat rel sub in
      let dir = Filename.concat dir rel in
      let pkg =
        try StringMap.find sub parent_index.pkgs
        with Not_found -> { pkg_name = sub; index = index_file sub }
      in
      let index = read (Filename.concat dir pkg.index) in
      let parent_index = {
        parent_index with pkgs = StringMap.add sub pkg parent_index.pkgs
      } in
      let acc = ((rel,pkg.index),index)::(parent_paths,parent_index)::ancs in
      descend acc rel more
  in
  match descend [] "" (Stringext.split ~on:'/' pkg) with
  | [] -> (("", index_file dir), read (index_file dir)), []
  | pkg_index::parents -> pkg_index, parents
