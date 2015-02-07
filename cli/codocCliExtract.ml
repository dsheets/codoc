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

let id x = x

let (/) = Filename.concat

let cmti_path path output = CodocUtil.(rel_of_path (depth output) path)

let only_cmti file path =
  if Filename.check_suffix file ".cmti"
  then begin
    true
  end
  else false

let xml_filename_of_cmti cmti =
  Filename.(chop_suffix (basename cmti) ".cmti")^".xml"
let dir_of_cmti cmti = Filename.(chop_suffix (basename cmti) ".cmti")
let xml_index_of_cmti cmti = (dir_of_cmti cmti) / "index.xml"

let all_cmtis dir =
  CodocSysUtil.foldp_paths (fun lst rel_cmti -> rel_cmti::lst) only_cmti [] dir

let exists_package dir package rel_file =
  let path = dir / package / rel_file in
  if Sys.file_exists path then Some path else None

let extract ~force ~index cmti out_dir rel_xml =
  let xml = out_dir / rel_xml in
  if not force && Sys.file_exists xml
  then Error.use_force xml
  else
    let dirs = (Dir.name xml)::(
      if index then [out_dir / CodocConfig.rel_index_xml] else []
    ) in
    (* here, we rely on umask to set the perms correctly *)
    match Dir.make_dirs_exist ~perm:0o777 dirs with
    | Some err -> err
    | None ->
      let rel_cmti = cmti_path cmti xml in
      let xml_file = Filename.basename rel_xml in
      let root_fn unit_name unit_digest =
        let open CodocDoc in
        let cmti = { cmti_path = rel_cmti; unit_name; unit_digest } in
        Xml (xml_file, Cmti cmti)
      in
      let open DocOck in
      match read_cmti root_fn cmti with
      | Not_an_interface -> Error.not_an_interface cmti
      | Wrong_version_interface -> Error.wrong_version_interface cmti
      | Corrupted_interface -> Error.corrupted_interface cmti
      | Not_a_typedtree -> Error.not_a_typedtree cmti
      | Ok unit ->
        let _root, mod_name = CodocUtil.root_of_unit unit in
        let oc = open_out xml in
        let output = Xmlm.make_output (`Channel oc) in
        let printer = DocOckXmlPrint.build (fun output root ->
          Xmlm.output_tree id output (List.hd (CodocDoc.xml_of_root root))
        ) in
        DocOckXmlPrint.file printer output unit;
        close_out oc;
        let open CodocIndex in
        let unit = {
          mod_name; xml_file = rel_xml; html_file = None;
          issues = [];
        } in
        if not index then `Ok unit
        else
          (* TODO: Use index caching? *)
          (* Creating *or* updating index so no need to check for force *)
          (* TODO: FIXME this can raise *)
          let index = read out_dir CodocConfig.rel_index_xml in
          let units = StringMap.add mod_name unit index.units in
          let index = { index with units } in
          write index;
          `Ok unit

let extract_package ~force ~index in_dir rel_cmti out_dir package =
  let rel_dir = Dir.name rel_cmti in
  let xml_file = xml_index_of_cmti rel_cmti in
  let cmti = in_dir / rel_cmti in
  extract ~force ~index cmti (out_dir / package) (rel_dir / xml_file)

let run_dir ~force ~index in_dir out_dir package =
  let cmtis = all_cmtis in_dir in
  let cmti_count = List.length cmtis in
  Printf.printf
    "%4d cmti under %s\n" cmti_count in_dir;
  match if force then [] else List.fold_left (fun errs rel_cmti ->
    let rel_dir = Dir.name rel_cmti in
    let xml_file = xml_index_of_cmti rel_cmti in
    match exists_package out_dir package (rel_dir / xml_file) with
    | None -> errs
    | Some path -> (Error.use_force path)::errs
  ) [] cmtis with
    | (_::_) as errs -> CodocCli.combine_errors errs
    | [] -> match List.fold_left (fun (units,errs) rel_cmti ->
      let rel_dir = Dir.name rel_cmti in
      let xml_file = xml_index_of_cmti rel_cmti in
      let index = false in
      match if package = ""
        then
          extract ~force ~index (in_dir / rel_cmti) out_dir (rel_dir / xml_file)
        else
          extract_package ~force ~index in_dir rel_cmti out_dir package
      with
      | `Ok unit -> (unit::units, errs)
      | `Error err -> (units, (`Error err)::errs)
    ) ([],[]) cmtis with
      | _, ((_::_) as errs) -> CodocCli.combine_errors errs
      | [], [] -> `Ok (`Dir out_dir)
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
            StringMap.add unit.mod_name unit map
          ) pkg_index.units units in
          let pkg_index = { pkg_index with units } in
          write pkg_index;
          List.iter (fun (_name, index) -> write index) pkg_parents;
          `Ok (`Dir out_dir)

let extract_file ~force ~index in_file out_dir xml_file package =
  if package = ""
  then if index
    then Error.no_file_index
    else
      CodocCli.map_ret (fun _ -> ())
        (extract ~force ~index in_file out_dir xml_file)
  else Error.no_file_package

let run ({ CodocCli.Common.force; index }) output path package =
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, _ when not (Filename.check_suffix in_file ".cmti") ->
    `Error (false, "source "^in_file^" is not a cmti")
  | `File in_file, None ->
    let xml_file = xml_filename_of_cmti in_file in
    let out_dir = Dir.name in_file in
    CodocCli.map_ret
      (fun () -> `File xml_file)
      (extract_file ~force ~index in_file out_dir xml_file package)
  | `File in_file, Some (`Missing out_file | `File out_file) ->
    (* simple doc gen *)
    let out_dir = Dir.name out_file in
    let rel_file = Filename.basename out_file in
    CodocCli.map_ret
      (fun () -> `File out_file)
      (extract_file ~force ~index in_file out_dir rel_file package)
  | `File in_file, Some (`Dir out_dir) ->
    if package = ""
    then CodocCli.map_ret (fun _ -> `Dir out_dir)
      (extract ~force ~index in_file out_dir (xml_index_of_cmti in_file))
    else
      let in_dir = Dir.name in_file in
      let rel_file = Filename.basename in_file in
      CodocCli.map_ret (fun _ -> `Dir out_dir)
        (extract_package ~force ~index in_dir rel_file out_dir package)
  | `Dir in_dir, None -> run_dir ~force ~index in_dir in_dir package
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    run_dir ~force ~index in_dir out_dir package
  | `Dir in_dir, Some (`File out_file) -> Error.dir_to_file in_dir out_file
