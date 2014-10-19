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

open Cmdliner
open Webmaster_cli

let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "David Sheets <sheets@alum.mit.edu>";
  `S "BUGS";
  `P "Browse and report new issues at"; `Noblank;
  `P "<https://github.com/dsheets/ocamlary/issues>.";
]

let doc_cmd =
  let doc = "produce module interface documentation" in
  let man = [

  ] @ help_sections
  in
  let path' = path ~doc:"the module, interface, or directory to document" 0 in
  Term.(ret (pure OcamlaryDoc.generate
               $ common $ OcamlaryCli.format $ output $ path'),
        info "doc" ~doc ~sdocs:global_option_section ~man)

let default_cmd =
  let doc = "produce documentation of OCaml modules" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b, ocamlary) produces documentation of OCaml modules.";
  ] @ help_sections
  in
  let exec_name = Filename.basename Sys.argv.(0) in
  let no_cmd_err _ = `Error (true, "No command specified.") in
  Term.(ret (pure no_cmd_err $ common),
        info exec_name ~version:"0.1.0" ~sdocs:global_option_section
          ~doc ~man)

;;

match Term.eval_choice default_cmd [
  doc_cmd;
] with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
