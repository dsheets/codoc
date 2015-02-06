#!/usr/bin/env ocaml

#use "topfind"
#require "unix"

let (/) = Filename.concat

let webserver = "cohttp-server-lwt"

let get_line cmd =
  let ic = Unix.open_process_in cmd in
  let line = input_line ic in
  close_in ic;
  line

let serve_flag = ref false
let opam_root = get_line "opam config var root"
let switch = get_line "opam switch show"
let compiler = get_line "opam config var compiler"

let get_package_version pkg = get_line ("opam show -f version "^pkg)

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

let build_attempted = ref false
let build_doc output =
  build_attempted := true;
  let doc_build = opam_root / switch / "build" in
  let options = if !serve_flag
    then "--index -f --scheme http"
    else "--index -f --scheme file"
  in
  match Unix.system
    ("codoc doc "^options^" --package ocaml."^compiler^" "
     ^doc_build^"/ocaml -o "^output)
  with
  | Unix.WSIGNALED _ -> failwith "building ocaml's docs was killed by signal"
  | Unix.WSTOPPED _ -> failwith "building ocaml's docs was stopped by signal"
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
    if !serve_flag then begin
      Unix.chdir output;
      print_endline
        "\nStarting web server for documentation at http://localhost:8080/";
      Unix.execvp webserver [|webserver|]
    end
    else
      if output.[0] = '/'
      then print_endline ("\nDone. file://"^output)
      else print_endline
        ("\nDone. file://"^((Unix.getcwd ()) / output / "index.html"))
  | Unix.WEXITED k ->
    failwith ("building ocaml's docs exited with "^(string_of_int k))

;;

let cli_spec = [
  "--serve", Arg.Set serve_flag, "exec "^webserver^" in doc dir when built";
] in
let usage = "opam doc <output_directory>" in
Arg.parse cli_spec build_doc usage;
if not !build_attempted then Arg.usage cli_spec usage
