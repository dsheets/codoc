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

type file_type = CodocSysUtil.file_type = Interface | Index | Unknown

let (/) = Filename.concat

let inspect env in_file =
  let ic = open_in in_file in
  let input = Xmlm.make_input (`Channel ic) in
  DocOckXmlParse.(match file CodocXml.doc_parser input with
  | Error (start, pos, s) ->
    close_in ic;
    [CodocIndex.Xml_error (in_file, pos, s)]
  | Ok unit ->
    close_in ic;
    let root, name = CodocUtil.root_of_unit unit in
    CodocEnvironment.index env in_file name root unit;
    []
  )

(* a bit boring... we don't have any collection of sources for linking *)
let link_file ~force ~index in_file out_file package =
  if package = ""
  then if index then Error.no_file_index
    else
      if not force && Sys.file_exists out_file
      then Error.use_force out_file
      else begin
        let out_dir = Filename.dirname out_file in
        match Dir.make_exist ~perm:0o755 out_dir with
        | Some err -> err
        | None ->
          (* TODO: rewrite root *)
          CodocSysUtil.copy in_file out_file;
          `Ok ()
      end
  else Error.no_file_package

(* TODO: should read from out_dir's indexes *)
let link_package ~force ~index in_dir rel_file out_dir package =
  link_file ~force ~index (in_dir / rel_file) (out_dir / package / rel_file) ""

let check_create_safe ~force index out_dir = CodocIndex.(
  fold_down_units
    ~unit_f:(fun errs index ({ xml_file }) ->
      let path = Filename.dirname (out_dir / index.path) / xml_file in
      if not force && Sys.file_exists path
      then (Error.use_force path)::errs
      else
        match Dir.make_exist ~perm:0o755 (Filename.dirname path) with
        | Some err -> err::errs
        | None -> errs
    )
    [] index
)

(* TODO: should read from out_dir's indexes *)
let run_index ~force ~index in_index out_dir package =
  let root = Filename.dirname in_index in
  let path = Filename.basename in_index in
  let idx = CodocIndex.read root path in
  let env = CodocEnvironment.create idx in
  let pkg_index = CodocIndex.goto idx package in
  let errs = check_create_safe ~force pkg_index out_dir in
  match errs with
  | (_::_) -> CodocCli.combine_errors errs
  | [] ->
    let open CodocIndex in
    let unit_f (units,errs) index gunit =
      let { xml_file } = gunit in
      let index_dir = match Filename.dirname index.path with
        | "." -> ""
        | p -> p
      in
      let unit_path = index_dir / xml_file in
      let root_unit_path = index.root / unit_path in
      let ic = open_in root_unit_path in
      let input = Xmlm.make_input (`Channel ic) in
      DocOckXmlParse.(match file CodocXml.doc_parser input with
      | Error (start, pos, s) ->
        close_in ic;
        let issues = [CodocIndex.Xml_error (root_unit_path, pos, s)] in
        ((root_unit_path, None, index.path, gunit, issues)::units,
         errs)
      | Ok unit ->
        close_in ic;
        let root, mod_name = CodocUtil.root_of_unit unit in
        CodocEnvironment.index env unit_path mod_name root unit;
        (* TODO: rewrite root? *)
        let unit = DocOck.resolve (CodocEnvironment.resolver env) unit in
        let issues = List.rev_map (fun mod_name ->
          Module_resolution_failed mod_name
        ) (CodocEnvironment.failures_of_root env root) in
        let out_path = out_dir / unit_path in
        ((out_path, Some unit, index.path, gunit, issues)::units,
         errs)
      )
    in match fold_down_units ~unit_f ([],[]) pkg_index with
    | (_,((_::_) as errs)) -> CodocCli.combine_errors errs
    | (units,[]) ->
      List.iter (function
        | (path, Some unit, _src_index, _gunit, _issues) ->
          let oc = open_out path in
          let output = Xmlm.make_output (`Channel oc) in
          DocOckXmlFold.((file { f = CodocXml.doc_printer }).f)
            (fun () signal -> Xmlm.output output signal) () unit;
          close_out oc
        | (_, None, _, _, _) -> ()
      ) units;
      let errs = List.fold_left
        (fun errs (path, _unit_opt, src_index, gunit, unit_issues) ->
          if index then
            let in_index = read_cache pkg_index src_index in
            let out_index = read_cache
              { in_index with root = out_dir } src_index
            in
            let unit_issues = List.rev_append gunit.unit_issues unit_issues in
            let index = set_gunit out_index { gunit with unit_issues } in
            let index = add_packages in_index index in
            write_cache index;
            errs
          else (List.map (error_of_unit_issue path) unit_issues)@errs
        ) [] units
      in
      (if index then flush_cache pkg_index);
      match errs with
      | [] -> `Ok ()
      | errs ->
        List.iter (fun (`Error (_,msg)) -> prerr_endline msg) errs; `Ok ()

let run ({ CodocCli.Common.force; index }) output path package =
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, None -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface -> `Ok () (* TODO: do something? rewrite root? *)
      | Index ->
        let out_dir = Filename.dirname in_file in
        run_index ~force ~index in_file out_dir package
    end
  | `File in_file, Some (`Missing out_file) ->
    begin match CodocSysUtil.deduce_file_type in_file with
    | Unknown -> Error.unknown_file_type in_file
    | Interface -> link_file ~force ~index in_file out_file package
    | Index -> run_index ~force ~index in_file out_file package
    end
  | `File in_file, Some (`File out_file) ->
    begin match CodocSysUtil.deduce_file_type in_file with
    | Unknown -> Error.unknown_file_type in_file
    | Interface -> link_file ~force ~index in_file out_file package
    | Index -> Error.index_to_file in_file out_file
    end
  | `File in_file, Some (`Dir out_dir) ->
    begin match CodocSysUtil.deduce_file_type in_file with
    | Unknown -> Error.unknown_file_type in_file
    | Interface ->
      if package = ""
      then
        let basename = Filename.basename in_file in
        link_file ~force ~index in_file (out_dir / basename) package
      else
        let in_dir = Filename.dirname in_file in
        let rel_file = Filename.basename in_file in
        link_package ~force ~index in_dir rel_file out_dir package
    | Index -> run_index ~force ~index in_file out_dir package
    end
  | `Dir in_dir, None ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) -> `Ok () (* TODO: do something? rewrite root? *)
    | Some (source, Index) -> run_index ~force ~index source in_dir package
    end
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      let file_name = Filename.basename source in
      (* TODO: package case could have meaning? *)
      link_file ~force ~index source (out_dir / file_name) package
    | Some (source, Index) -> run_index ~force ~index source out_dir package
    end
  | `Dir in_dir, Some (`File out_file) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      link_file ~force ~index source out_file package
    | Some (source, Index) -> Error.index_to_file source out_file
    end
