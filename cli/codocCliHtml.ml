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

let share_dir = ref CodocConfig.share_dir

let index_template = Lazy.from_fun CodocTemplate.(fun () ->
  load !share_dir index
)
let interface_template = Lazy.from_fun CodocTemplate.(fun () ->
  load !share_dir interface
)

let html_name_of = CodocUnit.Href.html_name_of

let write_html ~css ~title html_file templ path =
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

let write_substruct ~css ~title html html_file =
  let html = Blueprint.Tree.of_cons "data" html in
  let template = Lazy.force interface_template in
  let html = Blueprint.Scope.overlay html template in
  write_html ~css ~title html_file html "interface"

let update_css id css = CodocUnit.Href.ascent_of_ident id ^ css

let issues_of_doc_errors = List.map (fun x -> CodocIndex.Doc_error x)

let resolve_html unit_file issues f = function
  | None -> failwith "invariant violation resolve_html" (* TODO: ? *)
  | Some html_uri ->
    let html_path = Uri.(resolve "" (of_string unit_file) html_uri) in
    let out_file = Uri.to_string html_path in
    let issues = (f out_file)@issues in
    CodocIndex.({ html_file = Some (Uri.to_string html_uri); issues; })

let render_class scheme unit_file pkg_root css (uri_opt,html) c =
  let open DocOckTypes in
  let open DocOckPaths in
  let id = c.Class.id in
  let doc_errors = CodocAnalysis.of_class c in
  let issues = issues_of_doc_errors doc_errors in
  let title = Identifier.name id in
  let css = update_css (Identifier.any id) css in
  resolve_html unit_file issues (write_substruct ~css ~title html) uri_opt

let render_classtype scheme unit_file pkg_root css (uri_opt,html) c =
  let open DocOckTypes in
  let open DocOckPaths in
  let id = c.ClassType.id in
  let doc_errors = CodocAnalysis.of_classtype c in
  let issues = issues_of_doc_errors doc_errors in
  let title = Identifier.name id in
  let css = update_css (Identifier.any id) css in
  resolve_html unit_file issues (write_substruct ~css ~title html) uri_opt

let render_module scheme unit_file pkg_root css (uri_opt,html) m =
  let open DocOckTypes in
  let open DocOckPaths in
  let id = m.Module.id in
  let doc_errors = CodocAnalysis.of_module m in
  let issues = issues_of_doc_errors doc_errors in
  let title = Identifier.name id in
  let css = update_css (Identifier.any id) css in
  resolve_html unit_file issues (write_substruct ~css ~title html) uri_opt

let render_moduletype scheme unit_file pkg_root css (uri_opt,html) m =
  let open DocOckTypes in
  let open DocOckPaths in
  let id = m.ModuleType.id in
  let doc_errors = CodocAnalysis.of_moduletype m in
  let issues = issues_of_doc_errors doc_errors in
  let title = Identifier.name id in
  let css = update_css (Identifier.any id) css in
  resolve_html unit_file issues (write_substruct ~css ~title html) uri_opt

let render_substruct scheme unit_file pkg_root css = CodocUnit.Substruct.(
  let base = None in
  let html_map = CodocUnitHtml.of_substruct_map scheme pkg_root base in
  compose
    (product (homo_map (fun x -> x)) html_map)
    {
      map_class = render_class scheme unit_file pkg_root css;
      map_classtype = render_classtype scheme unit_file pkg_root css;
      map_module = render_module scheme unit_file pkg_root css;
      map_moduletype = render_moduletype scheme unit_file pkg_root css;
    }
)

let render_substructs scheme unit_file pkg_root css sub =
  CodocUnit.Substruct.map (render_substruct scheme unit_file pkg_root css) sub

let prepare_create_interface ~force in_file out =
  let ic = open_in in_file in
  let input = Xmlm.make_input (`Channel ic) in
  match DocOckXmlParse.file CodocXml.doc_parser input with
  | DocOckXmlParse.Error (start, pos, s) ->
    close_in ic;
    let issue = CodocIndex.Xml_error (in_file, pos, s) in
    `Error [ CodocIndex.error_of_unit_issue in_file issue ]
  | DocOckXmlParse.Ok unit ->
    close_in ic;
    DocOckTypes.Unit.(match unit.content with
      | Pack _ -> (* TODO: support packs *)
        `Error [
          `Error (false, "packs not yet supported so cannot render "^in_file)
        ]
      | Module signature ->
        let substruct =
          CodocUnit.Substruct.root_of_unit_signature unit signature
        in
        match CodocUnit.Href.loc "file" substruct with
        | None -> `Error [
          `Error (false, "failed to construct interface loc")
        ]
        | Some loc ->
          match CodocUnit.Substruct.(
            map_of_unit (compose ident_map (homo_map (fun id ->
              CodocUnit.Href.of_ident loc (DocOckPaths.Identifier.any id)
            ))) unit
          ) with
          | None -> `Error [
            `Error (false,
                    "packs not yet supported so cannot render "^in_file)
          ]
          | Some substructs ->
            let f = CodocUnit.Substruct.list_option_fold () in
            let uris = CodocUnit.Substruct.fold f [] substructs in
            match List.fold_left (fun errs uri ->
              let out = Uri.(to_string (resolve "" (of_string out) uri)) in
              if not force && Sys.file_exists out
              then (Error.use_force out)::errs
              else match Dir.make_exist ~perm:0o755 (Filename.dirname out) with
                | Some err -> err::errs
                | None -> errs
            ) [] uris with
            | [] -> `Ok substructs
            | errs -> `Error errs
    )

let print_issues in_file = List.iter (fun issue ->
  let `Error (_,msg) = CodocIndex.error_of_issue in_file issue in
  prerr_endline msg
)

let print_substructs_issues substructs =
  ignore CodocUnit.Substruct.(
    map (homo_map (fun { CodocIndex.html_file; issues } ->
      let html_file = match html_file with None -> "<unknown>" | Some s -> s in
      print_issues html_file issues
    )) substructs)

let render_interface_ok ~force in_file out_file scheme css =
  match prepare_create_interface ~force in_file out_file with
  | `Error errs -> CodocCli.combine_errors errs
  | `Ok substructs ->
    let subs = render_substructs scheme out_file None css substructs in
    print_substructs_issues subs;
    `Ok ()

let render_index name index substructs out_file scheme css =
  let html =
    CodocIndexHtml.of_package ~name ~index ~substructs ~scheme
  in
  let html = Blueprint.Tree.of_cons "data" html in
  let html = Blueprint.Scope.overlay html (Lazy.force index_template) in
  (* TODO: fixme title *)
  let title = if name = "" then "Documentation Index" else name in
  let issues = write_html ~css ~title out_file html "index" in
  print_issues out_file issues;
  `Ok ()

module StringMap = Map.Make(String)

let check_create_safe ~force index out_dir = CodocIndex.(
  fold_down
    ~unit_f:(fun r index ({ xml_file; substructs }) ->
      let html_file = html_name_of xml_file in
      let path = match Filename.dirname index.path with "." -> "" | p -> p in
      let xml_file = out_dir / path / xml_file in
      let path = out_dir / path / html_file in
      match prepare_create_interface ~force xml_file path with
      | `Error es -> begin match r with
        | `Error errs -> `Error (es@errs)
        | `Ok _ -> `Error es
      end
      | `Ok substructs -> match r with
        | `Error errs -> `Error errs
        | `Ok others -> `Ok (StringMap.add path substructs others)
    )
    ~pkg_f:(fun rc r index ->
      let html_file = html_name_of index.path in
      let path = out_dir / html_file in
      if not force && Sys.file_exists path
      then begin match r with
        | `Error errs -> rc (`Error ((Error.use_force path)::errs))
        | `Ok _ -> rc (`Error [ Error.use_force path ])
      end
      else match Dir.make_exist ~perm:0o755 (Filename.dirname path) with
        | None -> rc r
        | Some err -> match r with
          | `Error errs -> `Error (err::errs) (* don't recurse *)
          | `Ok _ -> `Error [ err ]
    )
    (`Ok StringMap.empty) index
)

let render_dir ~force ~index in_index out_dir scheme css =
  let root = Filename.dirname in_index in
  let path = Filename.basename in_index in
  let idx = CodocIndex.read root path in
  match check_create_safe ~force idx out_dir with
  | `Error errs -> CodocCli.combine_errors errs
  | `Ok subs ->
    let open CodocIndex in
    let subcache = Hashtbl.create 10 in
    let unit_f idxs idx gunit =
      let path = match Filename.dirname idx.path with "." -> "" | p -> p in
      let html_file = html_name_of gunit.xml_file in
      let pkg_root = Some CodocUtil.(ascent_of_depth "" (depth html_file)) in
      let html_path = path / html_file in
      let css = CodocUtil.(ascent_of_depth css (depth html_path)) in
      let html_root = out_dir / html_path in
      let subs = StringMap.find html_root subs in (* TODO: shouldn't exn... *)
      let substructs = render_substructs scheme html_root pkg_root css subs in
      Hashtbl.add subcache (out_dir / idx.path) substructs;
      if index
      then
        let out_index = read_cache { idx with root = out_dir } idx.path in
        let substructs = CodocUnit.Substruct.to_name substructs in
        let index = set_gunit out_index { gunit with substructs } in
        write_cache index;
        idxs
      else (print_substructs_issues substructs; idxs)
    in
    let pkg_f rc idxs idx = if index then rc (idx::idxs) else rc idxs in
    (* TODO: errors? XML errors? *)
    let idxs = fold_down ~unit_f ~pkg_f [] idx in
    List.iter (fun idx ->
      let subs = Hashtbl.find_all subcache (out_dir / idx.path) in
      let idx = read_cache { idx with root = out_dir } idx.path in
      let html_file = html_name_of idx.path in
      let path = out_dir / html_file in
      let name = match Filename.dirname idx.path with
        | "." -> ""
        | dir -> dir
      in
      let css = CodocUtil.(ascent_of_depth css (depth idx.path)) in
      let `Ok () = render_index name idx subs path scheme css in
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
    match Dir.make_exist ~perm:0o755 target_dir with
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
      match Dir.make_exist ~perm:0o755 (Filename.dirname out_file) with
      | Some err -> err
      | None ->
        let root = Filename.dirname in_file in
        let path = Filename.basename in_file in
        let index = CodocIndex.read root path in
        let render_f = render_index "" index [] out_file scheme in
        render_with_css ~force share css_dir render_f css

let run ({ CodocCli.Common.force; index }) output path scheme css share =
  share_dir := share;
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
