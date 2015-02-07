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

let extract_pkg_doc doc_build output pkg =
  match get_package_version pkg with
  | `Ok pkg_version -> begin
    try
      let pkgv = pkg^"."^pkg_version in
      let pkg_dir = doc_build / pkgv in
      (* TODO: If cmti only in _build, remove _build. *)
      check_system
        ("codoc extract -f --index --package "^pkgv^" "^pkg_dir^" -o "^output)
    with Not_found ->
      Printf.printf "%s: warning no build directory\n%!" pkg;
      `Ok ()
  end
  | `Error (help, msg) -> `Error (help, msg)

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

let extract_k doc_build output pkg k =
  match extract_pkg_doc doc_build output pkg with
  | `Ok () -> k ()
  | ret -> raise (Command_error ret)

let build_doc serve output =
  let doc_build = opam_root / switch / "build" in
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
              else extract_k doc_build output line next_line
            | k ->
              extract_k doc_build output (String.sub line 0 k) (fun () ->
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
