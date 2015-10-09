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

module Error = CodocCli.Error
module Dir = CodocSysUtil.Dir

let (/) = Filename.concat

let rel_path output file =
  CodocExtraction.relocate
    (fun root ->
     CodocUtil.(rel_of_path (depth output) root))
    file

let check_file ~force file =
  if not force && Sys.file_exists file then Some (Error.use_force file)
  else Dir.make_exist ~perm:0o755 (Dir.name file)

let check_source ~force ~out_dir ~package source =
  let rel_xml = CodocExtraction.rel_xml_source source in
  let path = out_dir / package / rel_xml in
  check_file ~force path

let read_file ~out_file file =
  let unit_path = rel_path out_file file in
  let root_fn unit_name unit_digest =
    let open CodocDoc in
    let cm = { unit_path; unit_name; unit_digest } in
    let out_base = Filename.basename out_file in
    Xml (out_base, Cm cm)
  in
  let path = CodocExtraction.path file in
  let open DocOck in
  match CodocExtraction.read root_fn file with
  | Not_an_interface -> Error.not_an_interface path
  | Wrong_version -> Error.wrong_version_interface path
  | Corrupted -> Error.corrupted_interface path
  | Not_a_typedtree -> Error.not_a_typedtree path
  | Not_an_implementation -> Error.not_an_implementation path
  | Ok unit -> `Ok unit

let read_source ~out_file source =
  let open DocOck.Types.Unit in
  match CodocExtraction.cmti source with
  | Some file ->
    let unit = read_file ~out_file file in
    Some (unit, file)
  | None ->
    match CodocExtraction.cmt source with
    | Some file -> begin
        match read_file ~out_file file with
        | `Error _ as err -> Some (err, file)
        | `Ok unit ->
          if unit.interface then Some (`Ok unit, file)
          else begin
            match CodocExtraction.cmi source with
            | Some file ->
              let unit = read_file ~out_file file in
              Some (unit, file)
            | None -> None
          end
      end
    | None ->
      match CodocExtraction.cmi source with
      | None -> None
      | Some file ->
        let unit = read_file ~out_file file in
        Some (unit, file)

let write_unit unit xml_out =
  let oc = open_out xml_out in
  let xml_out = Xmlm.make_output (`Channel oc) in
  DocOckXmlFold.((file { f = CodocXml.doc_printer }).f)
    (fun () signal -> Xmlm.output xml_out signal) () unit;
  close_out oc

let index_unit ~hidden ~file unit =
  let open CodocIndex in
  let root, name = CodocUtil.root_of_unit unit in
  let xml_file = CodocExtraction.rel_xml_file file in
  let html_files = None in
  let unit_issues =
    if CodocExtraction.is_cmi file then [ Cmi_source file ]
    else []
  in
  { name; root; xml_file; unit_issues; html_files; hidden; }

let extract_file ~index ~out_file file =
  match read_file ~out_file file with
  | `Error _ as err -> err
  | `Ok unit ->
    write_unit unit out_file;
    let unit = index_unit ~hidden:false ~file unit in
    match index with
    | None -> `Ok unit
    | Some idx_dir ->
      (* TODO: Use index caching? *)
      (* Creating *or* updating index so no need to check for force *)
      (* TODO: FIXME this can raise *)
      let open CodocIndex in
      let index = read idx_dir CodocConfig.rel_index_xml in
      let units = StringMap.add unit.name unit index.units in
      let index = { index with units } in
      write index;
      `Ok unit

let extract_source ~out_dir ~package source =
  let out_file =
    let rel_xml = CodocExtraction.rel_xml_source source in
    out_dir / package /rel_xml
  in
  match read_source ~out_file source with
  | None -> None
  | Some (`Error _ as err, _) -> Some err
  | Some (`Ok unit, file) ->
    let hidden =
      match CodocExtraction.cmi source with
      | None -> true
      | Some _ -> false
    in
    let unit = { unit with DocOck.Types.Unit.hidden } in
    write_unit unit out_file;
    let unit = index_unit ~hidden ~file unit in
    Some (`Ok unit)

let run_dir ~force ~index in_dir out_dir package =
  let extr = CodocSysUtil.collect in_dir in
  let sources = CodocExtraction.sources extr in
  let errs =
    List.fold_left
      (fun errs source ->
         match check_source ~force ~out_dir ~package source with
         | None -> errs
         | Some err -> err :: errs)
      [] sources
  in
  match errs with
  | _::_ -> CodocCli.combine_errors errs
  | [] ->
    let errs =
      List.fold_left
        (fun (units, errs) source ->
           let unit = extract_source ~out_dir ~package source in
           match unit with
           | None -> (units, errs)
           | Some (`Ok unit) -> (unit::units, errs)
           | Some (`Error err) -> (units, (`Error err)::errs))
        ([], []) sources
    in
    match errs with
    | _, ((_::_) as errs) -> CodocCli.combine_errors errs
    | units, [] ->
      if not index then `Ok (`Dir out_dir)
      else begin
        (* TODO: use index caching? *)
        let open CodocIndex in
        (* Creating *or* updating index so no need to check for force *)
        (* TODO: FIXME this can raise *)
        let rel_index = CodocConfig.rel_index_xml in
        let (pkg_path, pkg_index), pkg_parents =
          traverse ~rel_index out_dir package
        in
        let units =
          List.fold_left
            (fun map unit -> StringMap.add unit.name unit map)
            pkg_index.units units
        in
        let pkg_index = { pkg_index with units } in
        let dir = Dir.name (pkg_index.root / pkg_index.path) in
        match Dir.make_exist ~perm:0o755 dir with
        | Some err -> err
        | None ->
          write pkg_index;
          List.iter (fun (_name, index) -> write index) pkg_parents;
          Printf.printf "%s\n" (CodocExtraction.summarize extr);
          `Ok (`Dir out_dir)
      end

type file_output =
  | Dir of string
  | File of string
  | Unspecified

let check_file_output ~index ~package ~file = function
  | File out_file ->
    let err =
      if package <> "" then Some Error.no_file_package
      else if index then Some Error.no_file_index
      else None
    in
    err, out_file, None
  | Unspecified ->
    let err =
      if package <> "" then Some Error.no_file_package
      else if index then Some Error.no_file_index
      else None
    in
    err, CodocExtraction.xml_file file, None
  | Dir out_dir ->
    let rel_xml = CodocExtraction.rel_xml_file file in
    let out_file = out_dir / package / rel_xml in
    let index =
      if index then Some (out_dir / package)
      else None
    in
    None, out_file, index

let invalid_file path =
  `Error (false, "source " ^ path ^ " is not a cmti, cmt, or cmi")

let run_file ~force ~index ~package ~output in_file =
  let root = Filename.dirname in_file in
  let rel = Filename.basename in_file in
  match CodocExtraction.file ~root rel with
  | None -> invalid_file in_file
  | Some file ->
    match check_file_output ~index ~package ~file output with
    | Some err, _, _ -> err
    | None, out_file, index ->
      match check_file ~force out_file with
      | Some err -> err
      | None ->
        CodocCli.map_ret (fun _ -> `File out_file)
          (extract_file ~index ~out_file file)

let run ({ CodocCli.Common.force; index }) output path package =
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, None ->
    run_file ~force ~index ~package ~output:Unspecified in_file
  | `File in_file, Some (`File out_file) ->
    run_file ~force ~index ~package ~output:(File out_file) in_file
  | `File in_file, Some (`Dir out_dir) ->
    run_file ~force ~index ~package ~output:(Dir out_dir) in_file
  | `File in_file, Some (`Missing out_path) ->
    let output =
      if out_path.[String.length out_path - 1] = '/'
      then Dir out_path
      else File out_path
    in
    run_file ~force ~index ~package ~output in_file
  | `Dir in_dir, None ->
    run_dir ~force ~index in_dir in_dir package
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    run_dir ~force ~index in_dir out_dir package
  | `Dir in_dir, Some (`File out_file) ->
    Error.dir_to_file in_dir out_file
