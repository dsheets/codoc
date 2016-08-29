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

exception Command_error of unit Cmdliner.Term.ret

let (/) = Filename.concat

let version = CodocConfig.version

let webserver = "cohttp-server-lwt"

let get_line cmd =
  let ic = Unix.open_process_in cmd in
  try
    let line = input_line ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> `Ok line
    | _ -> `Error (false, "error with '"^cmd^"'") (* TODO: fixme *)
  with End_of_file -> `Error (false, "error with '"^cmd^"'") (* TODO: fixme *)

let get_all cmd =
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  try while true do
      let line = input_line ic in
      lines := line::!lines
    done;
    `Ok !lines (* never *)
  with End_of_file ->
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> `Ok (List.rev !lines)
    | _ -> `Error (false, "error with '"^cmd^"'") (* TODO: fixme *)

let or_die = function
  | `Ok line -> line
  | `Error (_, msg) -> prerr_endline msg; exit 1

let opam_root = or_die (get_line "opam config var root")
let switch = or_die (get_line "opam switch show")
let compiler = or_die (get_line "opam config var compiler")

let get_package_version pkg = get_line ("opam show -f version "^pkg)

(* TODO: fix error handling *)
let check_system cmd = Unix.(match system cmd with
  | WSIGNALED _ -> `Error (false, "'"^cmd^"' was killed by signal")
  | WSTOPPED _  -> `Error (false, "'"^cmd^"' was stopped by signal")
  | WEXITED 0 -> `Ok ()
  | WEXITED k ->
    `Error (false, Printf.sprintf "'%s' exited with code %d\n%!" cmd k)
)

let begins_with prefix_len prefix s = String.sub s 0 prefix_len = prefix

let extract output pkg pkg_dir list =
  let prefix = "_build/" in
  let prefix_len = String.length prefix in
  let pkg_dir =
    if list <> [] && begins_with prefix_len prefix pkg_dir &&
       List.for_all (begins_with prefix_len prefix) list
    then String.sub pkg_dir prefix_len (String.length pkg_dir - prefix_len)
    else pkg_dir
  in
  match get_package_version pkg with
  | `Error err -> `Error err
  | `Ok v ->
    let pkgv = pkg^"."^v in
    check_system
      ("codoc extract -f --index --package "^pkgv^" "^pkg_dir^" -o "^output)

let extract_pkg_doc output pkg =
  match get_line ("opam config var "^pkg^":lib") with
  | `Error err -> `Error err
  | `Ok pkg_lib ->
    match get_all ("codoc list-extractions "^pkg_lib) with
    | `Error err -> `Error err
    | `Ok ((_::_) as list) ->
      extract output pkg pkg_lib list
    | `Ok [] ->
      match get_line ("opam config var "^pkg^":build") with
      | `Error err -> `Error err
      | `Ok pkg_build ->
        if Sys.file_exists pkg_build
        then match get_all ("codoc list-extractions "^pkg_build) with
          | `Error err -> `Error err
          | `Ok list ->
            extract output pkg pkg_build list
        else `Ok () (* TODO: log? warn? *)

let link_doc output = check_system ("codoc link --index -f "^output)

let render_doc output options =
  check_system ("codoc html "^output^" "^options)

let render serve output options =
  print_endline "\nLinking complete. Rendering...";
  match render_doc output options with
  | `Ok () ->
    if serve then begin
      Unix.chdir output;
      print_endline
        "\nStarting web server for documentation at http://localhost:8080/";
      Unix.execvp webserver [|webserver|]
    end
    else
      if output.[0] = '/'
      then (print_endline ("\nDone. file://"^output); `Ok ())
      else (print_endline
              ("\nDone. file://"^((Unix.getcwd ()) / output / "index.html"));
            `Ok ())
  | ret -> ret

let link serve output options =
  print_endline "\nExtraction complete. Linking...";
  match link_doc output with
  | `Ok () -> render serve output options
  | ret -> ret

let extract_k output pkg k =
  match extract_pkg_doc output pkg with
  | `Ok () -> k ()
  | ret -> raise (Command_error ret)

let build_doc serve output =
  let doc_build = opam_root / switch / "lib" in
  let options = if serve
    then "--index -f --scheme http"
    else "--index -f --scheme file"
  in
  match Unix.system
    ("codoc doc "^options^" --package ocaml."^compiler^" "
     ^doc_build^"/ocaml -o "^output)
  with
  | Unix.WSIGNALED _ ->
    `Error (false, "building ocaml's docs was killed by signal")
  | Unix.WSTOPPED _ ->
    `Error (false, "building ocaml's docs was stopped by signal")
  | Unix.WEXITED 0 ->
    let topo_pkgs = Unix.open_process_in "opam list -S -s" in
    begin match begin
      try
        let rec next_line () =
          let rec next_pkg line = match String.index line ' ' with
            | exception Not_found ->
              if line = ""
              then next_line ()
              else extract_k output line next_line
            | k ->
              extract_k output (String.sub line 0 k) (fun () ->
                next_pkg (String.sub line (k+1) (String.length line - k - 1))
              )
          in
          let line = input_line topo_pkgs in
          next_pkg line
        in
        next_line ()
      with
      | End_of_file -> begin match Unix.close_process_in topo_pkgs with
        | Unix.WEXITED 0 -> `Ok ()
        | _ -> `Error (false, "error with 'opam list -S -s'") (* TODO: fixme *)
      end
      | Command_error ret -> ret
    end with
    | `Ok () -> link serve output options
    | ret -> ret
 end
  | Unix.WEXITED k ->
    `Error (false, "building ocaml's docs exited with "^(string_of_int k))

open Cmdliner

let serve =
  let docv = "SERVE" in
  let doc =
    "if provided, documentation is generated for HTTP scheme and "^webserver^" is
automatically started"
  in
  Arg.(value (flag & info ~docv ~doc ["serve"]))

let output =
  let docv = "PATH" in
  let doc = "directory in which to generate documentation" in
  Arg.(required (pos 0 (some string) None & info ~docv ~doc []))

let cmd =
  let doc = "produce documentation of opam packages" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,opam doc) produces documentation of opam packages.";
  ] @ CodocCli.help_sections
  in
  let sdocs = CodocCli.global_option_section in
  Term.(ret (pure build_doc $ serve $ output),
        info "opam doc" ~version ~sdocs ~doc ~man)

;;

match Term.eval cmd with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
