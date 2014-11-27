(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open OcamlaryCli

let xml path xml_file =
  let root = OcamlaryDoc.Cmti (path, FindlibUnits.unit_name_of_path path) in
  let unit = DocOck.(match read_cmti root path with
    | Not_an_interface -> failwith (path^" is not an interface") (* TODO *)
    | Wrong_version_interface ->
      failwith (path^" has the wrong format version") (* TODO *)
    | Corrupted_interface -> failwith (path^" is corrupted") (* TODO *)
    | Not_a_typedtree -> failwith (path^" is not a typed tree") (* TODO *)
    | Ok unit -> unit
  ) in
  let out_file = open_out xml_file in
  let output = Xmlm.make_output (`Channel out_file) in
  let printer = DocOckXmlPrint.build (fun output root ->
    Xmlm.output_tree (fun x -> x) output (List.hd (OcamlaryDoc.xml_of_root root))
  ) in
  DocOckXmlPrint.file printer output unit;
  close_out out_file;
  false

let html xml_file html_file =
  let in_file = open_in xml_file in
  let input = Xmlm.make_input (`Channel in_file) in
  let parse = DocOckXmlParse.build (fun input ->
    match Xmlm.input_tree
      ~el:OcamlaryDoc.root_of_xml
      ~data:OcamlaryDoc.data_of_xml
      input
    with None -> failwith "can't find root" (* TODO: fixme *)
    | Some root -> root
  ) in
  let unit = DocOckXmlParse.(match file parse input with
    | Ok unit -> unit
    | Error (None, pos', s)
    | Error (Some _, pos', s) -> failwith (xml_file^": "^s) (* TODO: pos *)
  ) in
  close_in in_file;
  let pathloc = OcamlaryDocHtml.pathloc (* TODO: fixme *)
    ~index_depth:0
    ~doc_base:(Uri.of_string "SOME_DOC_BASE")
    (DocOckPaths.Identifier.module_signature unit.DocOckTypes.Unit.id)
  in
  let html =
    OcamlaryDocHtml.of_unit ~pathloc unit
  in
  (* TODO: fixme *)
  let html = <:html<<html><head><link rel="stylesheet" type="text/css" href="file:///home/dsheets/Code/ocamlary/share/ocamlary.css"/></head><body>$html$</body></html>&>> in
  let out_file = open_out html_file in
  let output = Xmlm.make_output (`Channel out_file) in
  Htmlm.Xhtmlm.output_doc_tree output (List.hd html);
  close_out out_file;
  false

let only_cmti f path output =
  Printf.eprintf "%s\n%!" path;
  if Filename.check_suffix path ".cmti"
  then f path output
  else begin
    Printf.eprintf "Skipping non-cmti %s\n%!" path;
    true
  end

open Webmaster_cli

let generate ({ force }) formats (_output_links,output) (_path_links,path) =
  let cmd = "interface" in
  List.fold_left (fun _r -> function
  | Xml  ->
    let process path output =
      let base_name =
        if Filename.check_suffix output ".cmti"
        then Filename.chop_suffix output ".cmti"
        else output
      in
      let xml_file = base_name ^ ".xml" in
      xml path xml_file
    in
    Webmaster_file.output_of_input ~force ~cmd (only_cmti process) path output
  | Html ->
    let process path output =
      let base_name =
        if Filename.check_suffix output ".cmti"
        then Filename.chop_suffix output ".cmti"
        else output
      in
      let xml_file = base_name ^ ".xml" in
      let html_file = base_name ^ ".html" in
      xml path xml_file || html xml_file html_file
    in
    Webmaster_file.output_of_input ~force ~cmd (only_cmti process) path output
  ) (`Ok ()) formats
