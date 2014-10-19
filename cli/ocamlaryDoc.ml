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

let doc_state = Lazy.from_fun Ocamlary.load_state

let xml path xml_file =
  let cmti_info = Cmt_format.read_cmt path in
  let pkg = OpamPackage.of_string "_adhoc.1" in
  let lib = OpamLibrary.create pkg (OpamLibrary.Name.of_string "_adhoc") in
  let name =
    OpamDocName.Module.of_string cmti_info.Cmt_format.cmt_modname
  in
  let md = OpamDocPath.(Module.create (Lib lib) name) in
  let res _ = None in
  let tree = match cmti_info.Cmt_format.cmt_annots with
    | Cmt_format.Interface sg -> sg
    | _ -> failwith "not cmti... what should i do?" (* TODO *)
  in
  let intf = OpamDocCmti.read_interface_tree res md tree in
  let out_file = open_out xml_file in
  let output = Xmlm.make_output (`Channel out_file) in
  OpamDocXml.module_to_xml output intf;
  close_out out_file;
  false

let html xml_file html_file =
  let in_file = open_in xml_file in
  let input = Xmlm.make_input (`Channel in_file) in
  let modu = OpamDocXml.(module_of_xml { input; source=Some xml_file }) in
  close_in in_file;
  let pathloc = OcamlaryDocHtml.pathloc
    ~index_depth:0
    ~doc_base:(Uri.of_string "SOME_DOC_BASE")
    (OpamDocPath.Module modu.OpamDocTypes.path)
  in
  let doc_state = Lazy.force doc_state in
  let html =
    OcamlaryDocHtml.of_top_module doc_state.Ocamlary.doc_state ~pathloc modu
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
