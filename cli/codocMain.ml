(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

open Cmdliner
open CodocCli

let version = CodocConfig.version

let css_doc = "the URI reference of the CSS files to use"

let sdocs = global_option_section

let doc_cmd =
  let doc = "produce module interface documentation" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,codoc doc) is a single command interface to $(b,codoc)'s \
        functionality. This command is a composition of $(b,codoc extract), \
        $(b,codoc link), and $(b,codoc html). If you are integrating $(b,codoc) \
        into another system, you are advised to use those commands directly.";
  ] @ help_sections
  in
  let path' = path
    ~doc:"the module, interface, or directory to document"
    (Arg.pos 0)
  in
  Term.(ret (pure CodocCliDoc.generate
               $ Common.term $ output $ path'
               $ package $ scheme
               $ uri_ref ~doc:css_doc ["css"] $ share_dir),
        info "doc" ~doc ~sdocs ~man)

let list_extractions_cmd =
  let doc = "list files which will have documentation extracted" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,codoc list-extractions) finds all files which could have \
        documentation extracted that are present in the directory tree under \
        the target directory.";
  ] @ help_sections
  in
  let path' = path
    ~doc:"the directory to search"
    (Arg.pos 0)
  in
  Term.(ret (pure CodocCliListExtractions.run $ path'),
        info "list-extractions" ~doc ~sdocs ~man)

let extract_cmd =
  let doc = "extract documentation from cmt[i] files into XML" in
  let man = [

  ] @ help_sections
  in
  let path' = path
    ~doc:"the module, interface, or directory to extract"
    (Arg.pos 0)
  in
  Term.(ret (pure (map_ret (fun _ -> ())) $
               (pure CodocCliExtract.run
                  $ Common.term $ output $ path' $ package)),
        info "extract" ~doc ~sdocs ~man)

let link_cmd =
  let doc = "resolve XML documentation references" in
  let man = [

  ] @ help_sections
  in
  let path_doc = "the file, directory, or index hierarchy to link" in
  let path' = path ~doc:path_doc (Arg.pos 0) in
  Term.(ret (pure CodocCliLink.run
               $ Common.term $ output $ path' $ package),
        info "link" ~doc ~sdocs ~man)

let html_cmd =
  let doc = "render XML documentation into HTML" in
  let man = [

  ] @ help_sections
  in
  let path_doc = "the file, directory, or index hierarchy to render to HTML" in
  let path' = path ~doc:path_doc (Arg.pos 0) in
  Term.(ret (pure CodocCliHtml.run
               $ Common.term $ output $ path' $ package
               $ scheme $ uri_ref ~doc:css_doc ["css"] $ share_dir),
        info "html" ~doc ~sdocs ~man)

let default_cmd =
  let exec_name = Filename.basename Sys.argv.(0) in
  let doc = "produce documentation of OCaml modules" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, "^exec_name^") produces documentation of OCaml modules.");
  ] @ help_sections
  in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ Common.term),
        info exec_name ~version ~sdocs ~doc ~man)

;;

match Term.eval_choice default_cmd [
  doc_cmd;
  list_extractions_cmd;
  extract_cmd;
  link_cmd;
  html_cmd;
] with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
