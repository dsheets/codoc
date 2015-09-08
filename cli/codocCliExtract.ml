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

let hypot output root path = CodocUtil.(rel_of_path (depth output) root, path)

let rel_path fpath to_ = CodocExtraction.(uapply (hypot fpath) to_)

let exists_package dir package rel_file =
  let path = dir / package / rel_file in
  if Sys.file_exists path then Some path else None

let extract ~force ~index input out_dir rel_xml_out =
  let xml_out = out_dir / rel_xml_out in
  if not force && Sys.file_exists xml_out
  then Error.use_force rel_xml_out
  else
    let dirs = (Dir.name xml_out)::(
      if index then [out_dir / CodocConfig.rel_index_xml] else []
    ) in
    match Dir.make_dirs_exist ~perm:0o755 dirs with
    | Some err -> err
    | None ->
      let unit_path = rel_path xml_out input in
      let root_fn unit_name unit_digest =
        let open CodocDoc in
        let cm = { unit_path; unit_name; unit_digest } in
        let xml_file = Filename.basename xml_out in
        Xml (xml_file, Cm cm)
      in
      let open DocOck in
      let open CodocExtraction in
      match read root_fn input with
      | Not_an_interface -> Error.not_an_interface (path input)
      | Wrong_version -> Error.wrong_version_interface (path input)
      | Corrupted -> Error.corrupted_interface (path input)
      | Not_a_typedtree -> Error.not_a_typedtree (path input)
      | Not_an_implementation ->
        (* TODO: fixme *)
        failwith "unimplemented: Not_an_implementation"
      | Ok unit ->
        let root, name = CodocUtil.root_of_unit unit in
        let oc = open_out xml_out in
        let xml_out = Xmlm.make_output (`Channel oc) in
        DocOckXmlFold.((file { f = CodocXml.doc_printer }).f)
          (fun () signal -> Xmlm.output xml_out signal) () unit;
        close_out oc;
        let open CodocIndex in
        let xml_file = rel_xml_out in
        let html_files = None in
        let unit_issues =
          if CodocExtraction.is_cmti input
          then []
          else [ Non_cmti_source input ]
        in
        let hide = CodocExtraction.is_hidden input in
        let unit =
          { name; root; xml_file; unit_issues; html_files; hide; }
        in
        if not index then `Ok unit
        else
          (* TODO: Use index caching? *)
          (* Creating *or* updating index so no need to check for force *)
          (* TODO: FIXME this can raise *)
          let index = read out_dir CodocConfig.rel_index_xml in
          let units = StringMap.add name unit index.units in
          let index = { index with units } in
          write index;
          `Ok unit

let run_dir ~force ~index in_dir out_dir package =
  let extr = CodocCliListExtractions.collect in_dir in
  Printf.printf "%s\n" (CodocExtraction.summarize extr);
  let files = CodocExtraction.file_list extr in
  match if force then [] else List.fold_left (fun errs file ->
    match exists_package out_dir package (CodocExtraction.rel_xml file) with
    | None -> errs
    | Some path -> (Error.use_force path)::errs
  ) [] files with
    | (_::_) as errs -> CodocCli.combine_errors errs
    | [] -> match List.fold_left (fun (units,errs) file ->
      let index = false in
      let rel_xml = CodocExtraction.rel_xml file in
      match extract ~force ~index file (out_dir / package) rel_xml with
      | `Ok unit -> (unit::units, errs)
      | `Error err -> (units, (`Error err)::errs)
    ) ([],[]) files with
      | _, ((_::_) as errs) -> CodocCli.combine_errors errs
      | units, [] -> if not index then `Ok (`Dir out_dir)
        else
          (* TODO: use index caching? *)
          let open CodocIndex in
          (* Creating *or* updating index so no need to check for force *)
          (* TODO: FIXME this can raise *)
          let rel_index = CodocConfig.rel_index_xml in
          let (pkg_path, pkg_index), pkg_parents =
            traverse ~rel_index out_dir package
          in
          let units = List.fold_left (fun map unit ->
            StringMap.add unit.name unit map
          ) pkg_index.units units in
          let pkg_index = { pkg_index with units } in
          let dir = Dir.name (pkg_index.root / pkg_index.path) in
          match Dir.make_exist ~perm:0o755 dir with
          | Some err -> err
          | None ->
            write pkg_index;
            List.iter (fun (_name, index) -> write index) pkg_parents;
            `Ok (`Dir out_dir)

let extract_file ~force ~index file package out_dir rel_out =
  if package = ""
  then if index
    then Error.no_file_index
    else
      CodocCli.map_ret (fun _ -> ())
        (extract ~force ~index file out_dir rel_out)
  else Error.no_file_package

let file in_file f =
  let src = Filename.dirname in_file in
  let rel = Filename.basename in_file in
  match CodocExtraction.file ~src rel with
  | None -> `Error (false, "source "^in_file^" is not a cmti, cmt, or cmi")
  | Some file -> f file

let file_to_file ~force ~index in_file package out_file =
  (* simple doc gen *)
  file in_file (fun file ->
    let out_dir = Dir.name out_file in
    CodocCli.map_ret
      (fun () -> `File out_file)
      (extract_file ~force ~index file package out_dir out_file)
  )

let file_to_dir ~force ~index in_file package out_dir =
  file in_file (fun file ->
    let out_dir = out_dir / package in
    let out = CodocExtraction.relocate out_dir file in
    let rel_xml_out = CodocExtraction.rel_xml out in
    CodocCli.map_ret (fun _ -> `Dir out_dir)
      (extract ~force ~index file out_dir rel_xml_out)
  )

let run ({ CodocCli.Common.force; index }) output path package =
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, None -> file in_file (fun file ->
    let out_dir = Dir.name in_file in
    let xml_file = CodocExtraction.rel_xml file in
    CodocCli.map_ret
      (fun () -> `File xml_file)
      (extract_file ~force ~index file package out_dir xml_file)
  )
  | `File in_file, Some (`File out_file) ->
    file_to_file ~force ~index in_file package out_file
  | `File in_file, Some (`Missing out_path) ->
    if out_path.[String.length out_path - 1] = '/'
    then file_to_dir ~force ~index in_file package out_path
    else file_to_file ~force ~index in_file package out_path
  | `File in_file, Some (`Dir out_dir) ->
    file_to_dir ~force ~index in_file package out_dir
  | `Dir in_dir, None -> run_dir ~force ~index in_dir in_dir package
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    run_dir ~force ~index in_dir out_dir package
  | `Dir in_dir, Some (`File out_file) -> Error.dir_to_file in_dir out_file
