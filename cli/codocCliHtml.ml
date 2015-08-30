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

let index_template = Lazy.from_fun CodocTemplate.(fun () -> load index)
let interface_template = Lazy.from_fun CodocTemplate.(fun () -> load interface)

let html_name_of = CodocUnit.Href.html_name_of

let write_html ~css ~title html_file templ path =
  (* TODO: polyglot! *)
  let vars = Blueprint.Tree.of_kv_string [
    "css", css;
    "title", title;
  ] in
  let vars = Blueprint.Scope.overlay vars templ in
  let templ = Blueprint.Scope.(match find templ path with
    | None -> Printf.eprintf "template path '%s' missing" path; exit 1
    | Some t -> t
  ) in
  let template = Blueprint.(default_rope (Scope.template templ)) in
  try
    Blueprint_unix.bind_to_file html_file vars template;
    []
  with Blueprint.Error err ->
    [ CodocIndex.Template_error (Blueprint.error_message err) ]

let render_class pathloc unit_dir css () c =
  let open DocOckTypes in
  let open DocOckPaths in
  let { Class.id } = c in
  CodocUnit.Href.id_of_ident pathloc (Identifier.any id)

let render_classtype pathloc unit_dir css () c =
  let open DocOckTypes in
  let open DocOckPaths in
  let { ClassType.id } = c in
  CodocUnit.Href.id_of_ident pathloc (Identifier.any id)

let render_module pathloc unit_dir css () m =
  let open DocOckTypes in
  let open DocOckPaths in
  let { Module.id } = m in
  CodocUnit.Href.id_of_ident pathloc (Identifier.any id)

let render_moduletype pathloc unit_dir css () m =
  let open DocOckTypes in
  let open DocOckPaths in
  let { ModuleType.id } = m in
  CodocUnit.Href.id_of_ident pathloc (Identifier.any id)

let render_substruct pathloc unit_dir css =
  CodocUnit.Substruct.({
    map_class = render_class pathloc unit_dir css;
    map_classtype = render_classtype pathloc unit_dir css;
    map_module = render_module pathloc unit_dir css;
    map_moduletype = render_moduletype pathloc unit_dir css;
  })

let render_unit ?pkg_root in_file out_file scheme css =
  let ic = open_in in_file in
  let input = Xmlm.make_input (`Channel ic) in
  match DocOckXmlParse.file CodocXml.doc_parser input with
  | DocOckXmlParse.Error (start, pos, s) ->
    close_in ic;
    [CodocIndex.Xml_error (in_file, pos, s)]
  | DocOckXmlParse.Ok unit ->
    close_in ic;
    let root, _ = CodocUtil.root_of_unit unit in
    (* TODO: use triangle for path, not assumption!!! don't keep stacking *)
    let html_root = CodocDoc.(match root with
      | Html (_,_) -> root
      | _ -> Html (Filename.basename out_file, root)
    ) in
    DocOckTypes.Unit.(match unit.content with
      | Pack _ -> [] (* TODO: support pack *)
      | Module signature ->
        let substruct =
          CodocUnit.Substruct.root_of_unit_signature unit signature
        in
        match CodocUnit.Href.loc ?pkg_root scheme substruct with
        | None -> [] (* TODO: proper error for failure to construct a loc *)
        | Some loc ->
          let html = CodocDocHtml.of_unit loc unit in
          let _, title = CodocUtil.root_of_unit unit in
          let html = Blueprint.Tree.of_cons "data" html in
          let template = Lazy.force interface_template in
          let html = Blueprint.Scope.overlay html template in
          let issues = write_html ~css ~title out_file html "interface" in

          let id = unit.DocOckTypes.Unit.id in
          let id = CodocDoc.Maps.replace_ident_module_root html_root id in
          let unit = { unit with DocOckTypes.Unit.id } in
          let oc = open_out in_file in
          let output = Xmlm.make_output (`Channel oc) in
          DocOckXmlFold.((file { f = CodocXml.doc_printer }).f)
            (fun () signal -> Xmlm.output output signal) () unit;
          close_out oc;
          issues
    )

let print_issues in_file = List.iter (fun issue ->
  let `Error (_,msg) = CodocIndex.error_of_issue in_file issue in
  prerr_endline msg
)

let render_interface_ok ~force in_file out_file scheme css =
  if not force && Sys.file_exists out_file
  then Error.use_force out_file
  else
    (* here, we rely on umask to set the perms correctly *)
    match Dir.make_exist ~perm:0o777 (Filename.dirname out_file) with
    | Some err -> err
    | None ->
      let issues =
        render_unit in_file (html_name_of in_file) scheme css
      in
      print_issues in_file issues; `Ok ()

let render_index name index out_file scheme css =
  let normal_uri = CodocUnit.Href.normal_uri_for_scheme scheme in
  let uri_of_path = CodocUnit.Href.uri_of_path ~scheme in
  let html = CodocIndexHtml.of_package ~name ~index ~normal_uri ~uri_of_path in
  let html = Blueprint.Tree.of_cons "data" html in
  let html = Blueprint.Scope.overlay html (Lazy.force index_template) in
  (* TODO: fixme title *)
  let title = if name = "" then "Documentation Index" else name in
  let issues = write_html ~css ~title out_file html "index" in
  print_issues out_file issues;
  `Ok ()

let check_create_safe ~force index out_dir = CodocIndex.(
  fold_down
    ~unit_f:(fun errs index ({ xml_file }) ->
      let html_file = html_name_of xml_file in
      let path = Filename.dirname (out_dir / index.path) / html_file in
      if not force && Sys.file_exists path
      then (Error.use_force path)::errs
      else
        (* here, we rely on umask to set the perms correctly *)
        match Dir.make_exist ~perm:0o777 (Filename.dirname path) with
        | Some err -> err::errs
        | None -> errs
    )
    ~pkg_f:(fun rc errs index ->
      let html_file = html_name_of index.path in
      let path = out_dir / html_file in
      if not force && Sys.file_exists path
      then rc ((Error.use_force path)::errs)
      else
        (* here, we rely on umask to set the perms correctly *)
        match Dir.make_exist ~perm:0o777 (Filename.dirname path) with
        | Some err -> err::errs (* don't recurse *)
        | None -> rc errs
    )
    [] index
)

let render_dir ~force ~index in_index out_dir scheme css =
  let root = Filename.dirname in_index in
  let path = Filename.basename in_index in
  let idx = CodocIndex.read root path in
  match check_create_safe ~force idx out_dir with
  | (_::_) as errs -> CodocCli.combine_errors errs
  | [] ->
    let open CodocIndex in
    let unit_f idxs idx gunit =
      let path = match Filename.dirname idx.path with "." -> "" | p -> p in
      let xml_file = idx.root / path / gunit.xml_file in
      let html_file = match gunit.html_file with
        | None -> html_name_of gunit.xml_file
        | Some html_file -> html_file
      in
      let pkg_root = CodocUtil.(ascent_of_depth "" (depth html_file)) in
      let html_path = path / html_file in
      let css = CodocUtil.(ascent_of_depth css (depth html_path)) in
      let html_root = out_dir / html_path in
      (*let render_substruct = render_substruct pathloc out_dir path css in
      let substructs =
        CodocUnit.Substruct.map_of_unit render_substruct unit in*)
      let issues = render_unit ~pkg_root xml_file html_root scheme css in
      if index
      then
        let out_index = read_cache { idx with root = out_dir } idx.path in
        let issues = issues@gunit.issues in
        let html_file = Some html_file in
        let index = set_gunit out_index { gunit with issues; html_file; } in
        write_cache index;
        idxs
      else (print_issues xml_file issues; idxs)
    in
    let pkg_f rc idxs idx = if index then rc (idx::idxs) else rc idxs in
    (* TODO: errors? XML errors? *)
    let idxs = fold_down ~unit_f ~pkg_f [] idx in
    List.iter (fun idx ->
      let idx = read_cache { idx with root = out_dir } idx.path in
      let html_file = html_name_of idx.path in
      let path = out_dir / html_file in
      let name = match Filename.dirname idx.path with
        | "." -> ""
        | dir -> dir
      in
      let css = CodocUtil.(ascent_of_depth css (depth idx.path)) in
      let `Ok () = render_index name idx path scheme css in
      ()
    ) idxs;
    flush_cache idx;
    `Ok ()

let maybe_copy ~force path target_dir =
  let file_name = Filename.basename path in
  let target = target_dir / file_name in
  if not force && Sys.file_exists target
  then
    let path_digest = Digest.file path in
    let target_digest = Digest.file target in
    if path_digest <> target_digest
    then Error.use_force target
    else `Ok file_name
  else
    (* here, we rely on umask to set the perms correctly *)
    match Dir.make_exist ~perm:0o777 target_dir with
    | Some err -> err
    | None ->
      CodocSysUtil.copy path target;
      `Ok file_name

let shared_css share = share / CodocConfig.css_name

let render_with_css ~force share css_dir render_f = function
  | Some css -> render_f (Uri.to_string css)
  | None ->
    let css = shared_css share in
    match maybe_copy ~force css css_dir with
    | `Ok css -> render_f css
    | `Error _ as err -> err

let render_file ~force in_file out_file scheme css share =
  let css_dir = Filename.dirname out_file in
  match CodocSysUtil.deduce_file_type in_file with
  | Unknown -> Error.unknown_file_type in_file
  | Interface ->
    let render_f = render_interface_ok ~force in_file out_file scheme in
    render_with_css ~force share css_dir render_f css
  | Index ->
    if not force && Sys.file_exists out_file
    then Error.use_force out_file
    else
      (* here, we rely on umask to set the perms correctly *)
      match Dir.make_exist ~perm:0o777 (Filename.dirname out_file) with
      | Some err -> err
      | None ->
        let root = Filename.dirname in_file in
        let path = Filename.basename in_file in
        let index = CodocIndex.read root path in
        let render_f = render_index "" index out_file scheme in
        render_with_css ~force share css_dir render_f css

let run ({ CodocCli.Common.force; index }) output path scheme css share =
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, None ->
    render_file ~force in_file (html_name_of in_file) scheme css share
  | `File in_file, Some (`Missing out_file | `File out_file) ->
    render_file ~force in_file out_file scheme css share
  | `File in_file, Some (`Dir out_dir) ->
    let html_name = html_name_of (Filename.basename in_file) in
    render_file ~force in_file (out_dir / html_name) scheme css share
  | `Dir in_dir, None ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      let html_name = html_name_of source in
      let render_f = render_interface_ok ~force source html_name scheme in
      render_with_css ~force share in_dir render_f css
    | Some (source, Index) ->
      let render_f = render_dir ~force ~index source in_dir scheme in
      render_with_css ~force share in_dir render_f css
    end
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      let html_name = out_dir / (html_name_of (Filename.basename source)) in
      let render_f = render_interface_ok ~force source html_name scheme in
      render_with_css ~force share out_dir render_f css
    | Some (source, Index) ->
      let render_f = render_dir ~force ~index source out_dir scheme in
      render_with_css ~force share out_dir render_f css
    end
  | `Dir in_dir, Some (`File out_file) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      let render_f = render_interface_ok ~force source out_file scheme in
      let css_dir = Filename.dirname out_file in
      render_with_css ~force share css_dir render_f css
    | Some (source, Index) -> Error.index_to_file source out_file
    end
