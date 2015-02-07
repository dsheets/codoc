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

let (/) = Filename.concat

let version = CodocConfig.version

let webserver = "cohttp-server-lwt"

let get_line cmd =
  let ic = Unix.open_process_in cmd in
  let line = input_line ic in
  close_in ic;
  line

let opam_root = get_line "opam config var root"
let switch = get_line "opam switch show"
let compiler = get_line "opam config var compiler"

let get_package_version pkg = get_line ("opam show -f version "^pkg)

(* TODO: fix error handling *)
let check_system cmd = Unix.(match system cmd with
  | WSIGNALED _ -> failwith ("'"^cmd^"' was killed by signal")
  | WSTOPPED _  -> failwith ("'"^cmd^"' was stopped by signal")
  | WEXITED 0 -> ()
  | WEXITED k ->
    Printf.printf "'%s' exited with code %d\n%!" cmd k
)

let extract_pkg_doc doc_build output pkg =
  try
    let pkgv = pkg^"."^(get_package_version pkg) in
    let pkg_dir = doc_build / pkgv in
    (* TODO: If cmti only in _build, remove _build. *)
    check_system
      ("codoc extract -f --index --package "^pkgv^" "^pkg_dir^" -o "^output)
  with Not_found ->
    Printf.printf "%s: error no build directory\n%!" pkg

let link_doc output = check_system ("codoc link --index -f "^output)

let render_doc output options =
  check_system ("codoc html "^output^" "^options)

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
    begin try while true do
        let pkg = input_line topo_pkgs in
        extract_pkg_doc doc_build output pkg
      done
      with End_of_file -> close_in topo_pkgs
    end;
    print_endline "\nExtraction complete. Linking...";
    link_doc output;
    print_endline "\nLinking complete. Rendering...";
    render_doc output options;
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
