#use "topfind"
#require "unix"

let opam_root =
  let root_ic = Unix.open_process_in "opam config var root" in
  let root = input_line root_ic in
  close_in root_ic;
  root

let get_package_version pkg =
  let v_ic = Unix.open_process_in ("opam show -f version "^pkg) in
  let v = input_line v_ic in
  close_in v_ic;
  v

let check_system cmd = Unix.(match system cmd with
  | WSIGNALED _ -> failwith ("'"^cmd^"' was killed by signal")
  | WSTOPPED _  -> failwith ("'"^cmd^"' was stopped by signal")
  | WEXITED 0 -> ()
  | WEXITED k ->
    Printf.eprintf "'%s' exited with code %d\n%!" cmd k
)

let build_pkg_doc doc_build output pkg =
  try
    let pkgv = pkg^"."^(get_package_version pkg) in
    let pkg_dir = Filename.concat doc_build pkgv in
    (* TODO: If cmti only in _build, remove _build. *)
    check_system ("codoc doc --package "^pkgv^" "^pkg_dir^" -o "^output)
  with Not_found ->
    Printf.eprintf "%s: error no build directory\n%!" pkg

let build_doc output =
  let doc_build = Filename.concat opam_root "doc/build" in
  match Unix.system
    ("codoc doc --package ocaml.4.02.1+doc "^doc_build^"/ocaml -o "^output)
  with
  | Unix.WSIGNALED _ -> failwith "building ocaml's docs was killed by signal"
  | Unix.WSTOPPED _ -> failwith "building ocaml's docs was stopped by signal"
  | Unix.WEXITED 0 ->
    let topo_pkgs = Unix.open_process_in "opam list -S -s" in
    begin try while true do
        let pkg = input_line topo_pkgs in
        build_pkg_doc doc_build output pkg
      done
      with End_of_file -> close_in topo_pkgs
    end
  | Unix.WEXITED k ->
    failwith ("building ocaml's docs exited with "^(string_of_int k))
;;

Arg.parse [] build_doc "opam doc [output_directory]"
