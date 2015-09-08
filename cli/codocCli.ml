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

let map f = Term.(app (pure f))
let ret_map f t = Term.ret (map f t)
let map_ret f = function
  | `Ok v -> `Ok (f v)
  | `Error (help,msg) as err -> err

let global_option_section = "COMMON OPTIONS"

let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "David Sheets <sheets@alum.mit.edu>";
  `S "BUGS";
  `P "Browse and report new issues at"; `Noblank;
  `P "<https://github.com/dsheets/codoc/issues>.";
]

module Common = struct
  type t = {
    force : bool;
    index : bool;
  }

  let create_t force index = { force; index; }
  let create ~force ~index = create_t force index

  let force_arg = Arg.(value (
    let docv = "FORCE" in
    let doc = "force the execution of the command" in
    flag & info ~docs:global_option_section ~docv ~doc ["f"]
  ))

  let index_arg = Arg.(value (
    let docv = "INDEX" in
    let doc =
      "whether to update indexes and the relative path of the index file"
    in
    flag & info ~docs:global_option_section ~docv ~doc ["index"]
  ))

  let term = Term.(pure create_t $ force_arg $ index_arg)
end

type path = [
| `File of string
| `Dir of string
| `Missing of string
]

let rec to_path path = Unix.(
  try match (stat path).st_kind with
  | S_DIR -> `Ok (`Dir path)
  | S_REG -> `Ok (`File path)
  | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK ->
    `Error (false, "unsupported file type")
  with
  | Unix_error (ENOENT,_,_) -> `Ok (`Missing path)
  | Unix_error (e,_,_) -> `Error (false, path^": "^(error_message e))
)

let path ~doc arg =
  Arg.(ret_map to_path (required (
    let docv = "PATH" in
    arg (some string) None & info ~docv ~doc []
  )))

let path_opt ~doc names =
  Arg.(ret_map (function
  | None -> `Ok None
  | Some x -> map_ret (fun x -> Some x) (to_path x)
  ) (value (
    let docv = "PATH" in
    Arg.opt (some string) None & info ~docv ~doc names
  )))

let output = path_opt ~doc:"the output path" ["o"]

let package = Arg.(value (
  let docv = "PACKAGE" in
  let doc  = "the name of the package being generated" in
  opt string "" & info ["package"] ~docv ~doc
))

let scheme = Arg.(value (
  let docv = "SCHEME" in
  let doc  = "the scheme used to browse the documentation" in
  let schemes = enum [
    "file", "file"; "http", "http";
  ] in
  opt schemes "http" & info ["scheme"] ~docv ~doc
))

let uri_ref ~doc names = Term.(app (pure (function
  | Some s -> Some (Uri.of_string s)
  | None -> None
)) Arg.(value (
  let docv = "URI_REFERENCE" in
  opt (some string) None & info names ~docv ~doc
)))

let share_dir = Arg.(value (
  let docv = "SHARE_DIR" in
  let doc  = "the shared resource directory" in
  opt dir CodocConfig.share_dir & info ~docv ~doc ["share"]
))

module Error = struct
  let source_missing path =
    `Error (false, "source "^path^" does not exist")

  let source_not_found dir =
    `Error (false, "couldn't find source in directory "^dir)

  let use_force path =
    `Error (false, "destination "^path^" exists; use -f to overwrite")

  let dir_to_file dir file =
    `Error (false, "can't process directory "^dir^" into file "^file)

  let index_to_file index file =
    `Error (false, "can't process index "^index^" into file "^file)

  let unknown_file_type path =
    `Error (false, "don't know how to handle file "^path)

  let not_an_index path =
    `Error (false, path^" is not an index")

  let not_an_interface path =
    `Error (false, path^" is not an interface")

  let wrong_version_interface path =
    `Error (false, path^" has the wrong format version")

  let corrupted_interface path =
    `Error (false, path^" is corrupted")

  let not_a_typedtree path =
    `Error (false, path^" is not a typed tree")

  let no_file_package =
    `Error (false, "cannot use package when targeting a file")

  let no_file_index =
    `Error (false, "cannot use index when targeting a file")
end

let combine_errors errs = `Error
  begin List.fold_left (fun (show_help,str) -> function
  | `Error (err_help,err_str) -> (err_help || show_help, str ^ "\n" ^ err_str)
  ) (false,"") errs
  end
