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

module BlueTree = Blueprint.Tree

module Error = CodocCli.Error
module Dir = CodocSysUtil.Dir

type file_type = CodocSysUtil.file_type = Interface | Index | Unknown

let (/) = Filename.concat

let share_dir = ref CodocConfig.share_dir

let index_template = lazy CodocTemplate.(load !share_dir index)

let interface_template = lazy CodocTemplate.(load !share_dir interface)

let html_name_of = CodocUnit.Href.html_name_of

let write_html ~css ~title html_file templ path =
  let vars = Blueprint.Tree.(of_kv_maybe [
    "css", Some (of_string css);
    "title", match title with None -> None | Some s -> Some (of_string s);
  ]) in
  let vars = Blueprint.Scope.overlay vars templ in
  let templ = Blueprint.Scope.(match find templ path with
    | None -> Printf.eprintf "template path '%s' missing" path; exit 1
    | Some t -> t
  ) in
  let template = Blueprint.(default_rope (Scope.template templ)) in
  match Blueprint_unix.bind_to_file html_file vars template with
  | () -> []
  | exception Blueprint.Error err ->
    [ CodocIndex.Template_error (Blueprint.error_message err) ]

let check_file ~force html_file =
  if not force && Sys.file_exists html_file
  then Some (Error.use_force html_file)
  else Dir.make_exist ~perm:0o755 (Filename.dirname html_file)

let files_of_id ~force ~unit_loc ~unit_file id =
  let uri =
    match CodocUnit.Href.of_ident unit_loc id with
    | None -> failwith "invariant violation module_interface" (* TODO: ? *)
    | Some uri -> uri
  in
  let file = Uri.to_string uri in
  let html_uri = Uri.(resolve "" (of_string unit_file) uri) in
  let html_file = Uri.to_string html_uri in
  match check_file ~force html_file with
  | Some err -> err
  | None -> `Ok (html_file, file)

type interface =
  | Class of CodocDoc.root DocOck.Types.Class.t * string * string
  | ClassType of CodocDoc.root DocOck.Types.ClassType.t * string * string
  | Module of CodocDoc.root DocOck.Types.Module.t
              * string * string * interface list
  | ModuleType of CodocDoc.root DocOck.Types.ModuleType.t
                  * string * string * interface list
  | Argument of (CodocDoc.root DocOck.Paths.Identifier.module_
                * CodocDoc.root DocOck.Types.ModuleType.expr)
                * string * string * interface list
  | Unit of CodocDoc.root DocOck.Types.Unit.t
              * string * string * interface list

let rec module_interface ~files_of_id ~expander md =
  let id = DocOck.Paths.Identifier.any md.DocOck.Types.Module.id in
  match files_of_id id with
  | `Error _ as err -> err
  | `Ok (html_file, file) ->
    let ex = DocOck.expand_module expander md in
    let children = expansion_interfaces ~files_of_id ~expander ex in
    match children with
    | `Error _ as err -> err
    | `Ok children -> `Ok (Module(md, html_file, file, children))

and module_type_interface ~files_of_id ~expander mty =
  let id = DocOck.Paths.Identifier.any mty.DocOck.Types.ModuleType.id in
  match files_of_id id with
  | `Error _ as err -> err
  | `Ok (html_file, file) ->
    let ex = DocOck.expand_module_type expander mty in
    let children = expansion_interfaces ~files_of_id ~expander ex in
    match children with
    | `Error _ as err -> err
    | `Ok children -> `Ok (ModuleType(mty, html_file, file, children))

and argument_interface ~files_of_id ~expander ((id, _) as arg) =
  let id = DocOck.Paths.Identifier.any id in
  match files_of_id id with
  | `Error _ as err -> err
  | `Ok (html_file, file) ->
    let ex = DocOck.expand_argument expander arg in
    let children = expansion_interfaces ~files_of_id ~expander ex in
    match children with
    | `Error _ as err -> err
    | `Ok children -> `Ok (Argument(arg, html_file, file, children))

and class_interface ~files_of_id cl =
  let id = DocOck.Paths.Identifier.any cl.DocOck.Types.Class.id in
  match files_of_id id with
  | `Error _ as err -> err
  | `Ok (html_file, file) -> `Ok (Class(cl, html_file, file))

and class_type_interface ~files_of_id clty =
  let id = DocOck.Paths.Identifier.any clty.DocOck.Types.ClassType.id in
  match files_of_id id with
  | `Error _ as err -> err
  | `Ok (html_file, file) -> `Ok (ClassType(clty, html_file, file))

and expansion_interfaces ~files_of_id ~expander ex =
  match ex with
  | None -> `Ok []
  | Some (DocOck.Signature sg) ->
    signature_interfaces ~files_of_id ~expander sg
  | Some (DocOck.Functor(args, sg)) ->
    functor_interfaces ~files_of_id ~expander args sg

and signature_interfaces ~files_of_id ~expander sg =
  let rec loop acc = function
    | [] -> `Ok (List.rev acc)
    | item :: rest -> begin
        let open DocOck.Types.Signature in
        match item with
        | Module md -> begin
            let md = module_interface ~files_of_id ~expander md in
            match md with
            | `Error _ as err -> err
            | `Ok md -> loop (md :: acc) rest
          end
        | ModuleType mty -> begin
            let mty = module_type_interface ~files_of_id ~expander mty in
            match mty with
            | `Error _ as err -> err
            | `Ok mty -> loop (mty :: acc) rest
          end
        | Class cl -> begin
            let cl = class_interface ~files_of_id cl in
            match cl with
            | `Error _ as err -> err
            | `Ok cl -> loop (cl :: acc) rest
          end
        | ClassType clty -> begin
            let clty = class_type_interface ~files_of_id clty in
            match clty with
            | `Error _ as err -> err
            | `Ok clty -> loop (clty :: acc) rest
          end
        | Include incl -> begin
            let items =
              match DocOck.expand_include expander incl with
              | None -> `Ok []
              | Some sg -> signature_interfaces ~files_of_id ~expander sg
            in
            match items with
            | `Error _ as err -> err
            | `Ok items -> loop (List.rev_append items acc) rest
          end
        | _ -> loop acc rest
      end
  in
  loop [] sg

and arguments_interfaces ~files_of_id ~expander args =
  let rec loop acc = function
    | [] -> `Ok (List.rev acc)
    | argo :: rest -> begin
        match argo with
        | None -> loop acc rest
        | Some arg -> begin
            let arg = argument_interface ~files_of_id ~expander arg in
            match arg with
            | `Error _ as err -> err
            | `Ok arg -> loop (arg :: acc) rest
          end
      end
  in
  loop [] args

and functor_interfaces ~files_of_id ~expander args sg =
  let args = arguments_interfaces ~files_of_id ~expander args in
  match args with
  | `Error _ as err -> err
  | `Ok args ->
    let sg = signature_interfaces ~files_of_id ~expander sg in
    match sg with
    | `Error _ as err -> err
    | `Ok sg -> `Ok (args @ sg)

let unit_interface ~force ~env ~unit_file unit =
  let id = DocOck.Paths.Identifier.any unit.DocOck.Types.Unit.id in
  let unit_loc =
    match CodocUnit.Href.loc "file" id with
    | None -> failwith "invariant violation unit_interface" (* TODO: ? *)
    | Some loc -> loc
  in
  let files_of_id id = files_of_id ~force ~unit_loc ~unit_file id in
  let expander = CodocEnvironment.expander env in
  match files_of_id id with
  | `Error _ as err -> err
  | `Ok (html_file, file) ->
    let children =
      match DocOck.expand_unit expander unit with
      | None -> `Ok []
      | Some sg -> signature_interfaces ~files_of_id ~expander sg
    in
    match children with
    | `Error _ as err -> err
    | `Ok children -> `Ok (Unit(unit, html_file, file, children))

let update_css id css = CodocUnit.Href.ascent_of_ident id ^ css

let issues_of_doc_errors = List.map (fun x -> CodocIndex.Doc_error x)

let title_of_id id =
  let id = DocOck.Paths.Identifier.any id in
  let name = DocOck.Paths.Identifier.name id in
  let path = CodocDoc.Maps.string_of_ident id in
  Printf.sprintf "%s (%s)" name path

let add_up ~loc tree = match CodocUnit.Href.up loc with
  | Some up_href ->
    let up_href = Uri.to_string up_href in
    BlueTree.(add "up" (of_cons "href" (of_string up_href)) tree)
  | None -> tree

let loc_of_id ~scheme ~pkg_root id =
  let id = DocOck.Paths.Identifier.any id in
  match CodocUnit.Href.loc ?pkg_root scheme id with
  | None -> failwith "invariant violation: loc_of_id" (* TODO: ? *)
  | Some loc -> loc

let write_interface ~css ~id ~html ~html_file =
  let id = DocOck.Paths.Identifier.any id in
  let html = BlueTree.root html in
  let html = Blueprint.Tree.of_cons "data" html in
  let template = Lazy.force interface_template in
  let html = Blueprint.Scope.overlay html template in
  let title = Some (title_of_id id) in
  let css = update_css id css in
  write_html ~css ~title html_file html "interface"

let rec render_interface ~scheme ~pkg_root ~css ~env = function
  | Module(md, html_file, file, children) ->
    let id = md.DocOck.Types.Module.id in
    let doc_errors = CodocAnalysis.of_module md in
    let doc_issues = issues_of_doc_errors doc_errors in
    let loc = loc_of_id ~scheme ~pkg_root id in
    let body = CodocDocHtml.of_module loc env md in
    let html = add_up ~loc (BlueTree.of_cons "module" body) in
    let template_issues = write_interface ~css ~id ~html ~html_file in
    let issues = template_issues @ doc_issues in
    let name = DocOck.Paths.Identifier.name id in
    let children =
      List.map (render_interface ~scheme ~pkg_root ~css ~env) children
    in
    CodocIndex.Module(name, file, issues, children)
  | ModuleType(mty, html_file, file, children) ->
    let id = mty.DocOck.Types.ModuleType.id in
    let doc_errors = CodocAnalysis.of_module_type mty in
    let doc_issues = issues_of_doc_errors doc_errors in
    let loc = loc_of_id ~scheme ~pkg_root id in
    let body = CodocDocHtml.of_module_type loc env mty in
    let html = add_up ~loc (BlueTree.of_cons "module-type" body) in
    let template_issues = write_interface ~css ~id ~html ~html_file in
    let issues = template_issues @ doc_issues in
    let name = DocOck.Paths.Identifier.name id in
    let children =
      List.map (render_interface ~scheme ~pkg_root ~css ~env) children
    in
    CodocIndex.ModuleType(name, file, issues, children)
  | Argument((id, _) as arg, html_file, file, children) ->
    let doc_errors = CodocAnalysis.of_argument arg in
    let doc_issues = issues_of_doc_errors doc_errors in
    let loc = loc_of_id ~scheme ~pkg_root id in
    let body = CodocDocHtml.of_argument loc env arg in
    let html = add_up ~loc (BlueTree.of_cons "argument" body) in
    let template_issues = write_interface ~css ~id ~html ~html_file in
    let issues = template_issues @ doc_issues in
    let name = DocOck.Paths.Identifier.name id in
    let children =
      List.map (render_interface ~scheme ~pkg_root ~css ~env) children
    in
    CodocIndex.Argument(name, file, issues, children)
  | Class(cl, html_file, file) ->
    let id = cl.DocOck.Types.Class.id in
    let doc_errors = CodocAnalysis.of_class cl in
    let doc_issues = issues_of_doc_errors doc_errors in
    let loc = loc_of_id ~scheme ~pkg_root id in
    let body = CodocDocHtml.of_class loc env cl in
    let html = add_up ~loc (BlueTree.of_cons "class" body) in
    let template_issues = write_interface ~css ~id ~html ~html_file in
    let issues = template_issues @ doc_issues in
    let name = DocOck.Paths.Identifier.name id in
    CodocIndex.Class(name, file, issues)
  | ClassType(clty, html_file, file) ->
    let id = clty.DocOck.Types.ClassType.id in
    let doc_errors = CodocAnalysis.of_class_type clty in
    let doc_issues = issues_of_doc_errors doc_errors in
    let loc = loc_of_id ~scheme ~pkg_root id in
    let body = CodocDocHtml.of_class_type loc env clty in
    let html = add_up ~loc (BlueTree.of_cons "class-type" body) in
    let template_issues = write_interface ~css ~id ~html ~html_file in
    let issues = template_issues @ doc_issues in
    let name = DocOck.Paths.Identifier.name id in
    CodocIndex.ClassType(name, file, issues)
  | Unit(unit, html_file, file, children) ->
    let id = unit.DocOck.Types.Unit.id in
    let doc_errors = CodocAnalysis.of_unit unit in
    let doc_issues = issues_of_doc_errors doc_errors in
    let loc = loc_of_id ~scheme ~pkg_root id in
    let body = CodocDocHtml.of_unit loc env unit in
    let html = add_up ~loc (BlueTree.of_cons "module" body) in
    let template_issues = write_interface ~css ~id ~html ~html_file in
    let issues = template_issues @ doc_issues in
    let name = DocOck.Paths.Identifier.name id in
    let children =
      List.map (render_interface ~scheme ~pkg_root ~css ~env) children
    in
    CodocIndex.Module(name, file, issues, children)

let read_unit in_file =
  let ic = open_in in_file in
  let input = Xmlm.make_input (`Channel ic) in
  match DocOckXmlParse.file CodocXml.doc_parser input with
  | DocOckXmlParse.Error (start, pos, s) ->
    close_in ic;
    let issue = CodocIndex.Xml_error (in_file, pos, s) in
    `Error [ CodocIndex.error_of_unit_issue in_file issue ]
  | DocOckXmlParse.Ok (unit : _ DocOck.Types.Unit.t)->
    close_in ic;
    `Ok unit

let render_interface_ok ~force in_file unit_file scheme css =
  match read_unit in_file with
  | `Error errs -> CodocCli.combine_errors errs
  | `Ok unit ->
    let env = CodocEnvironment.create_for_unit unit in
    match unit_interface ~force ~env ~unit_file unit with
    | `Error _ as err -> err
    | `Ok intf ->
      let html_files =
        render_interface ~scheme ~pkg_root:None ~css ~env intf
      in
      CodocIndex.print_html_file_issues html_files;
      `Ok ()

let check_index ~force out_dir idx=
  let html_file = html_name_of idx.CodocIndex.path in
  let path = out_dir / html_file in
  check_file ~force path

let render_index ~scheme ~css name idx out_file =
  let html = CodocIndexHtml.of_package ~name ~index:idx ~scheme in
  let html = Blueprint.Tree.of_cons "data" html in
  let html = Blueprint.Scope.overlay html (Lazy.force index_template) in
  (* TODO: fixme title *)
  let title = if name = "" then None else Some name in
  let issues = write_html ~css ~title out_file html "index" in
  CodocIndex.print_issues out_file issues;
  ()

let check_indices ~force ~index out_dir pkg_index =
  if not index then None
  else begin
    let add_error err = function
      | Some errs -> Some (err :: errs)
      | None -> Some [err]
    in
    let pkg_f rc acc idx =
      match check_index ~force out_dir idx with
      | Some err -> add_error err acc
      | None -> rc acc
    in
    CodocIndex.fold_down_pkgs ~pkg_f None pkg_index
  end

let render_indices ~index ~scheme ~css out_dir pkg_index =
  let open CodocIndex in
  if index then begin
    let pkg_f rc acc idx =
      let idx = read_cache { idx with root = out_dir } idx.path in
      let html_file = html_name_of idx.path in
      let path = out_dir / html_file in
      let name =
        match Filename.dirname idx.path with
        | "." -> ""
        | dir -> dir
      in
      let css = CodocUtil.(ascent_of_depth css (depth idx.path)) in
      render_index ~scheme ~css name idx path;
      rc acc
    in
      fold_down_pkgs ~pkg_f () pkg_index
  end

let check_parents ~force ~index out_dir parents =
  if not index then None
  else begin
    let add_error err = function
      | Some errs -> Some (err :: errs)
      | None -> Some [err]
    in
    List.fold_left
      (fun acc idx ->
         match check_index ~force out_dir idx with
         | Some err -> add_error err acc
         | None -> acc)
      None
      parents
  end

let render_parents ~index ~scheme ~css out_dir parents =
  let open CodocIndex in
  if index then begin
    List.iter
      (fun idx ->
         let idx = read_cache { idx with root = out_dir } idx.path in
         let html_file = html_name_of idx.path in
         let path = out_dir / html_file in
         let name =
           match Filename.dirname idx.path with
           | "." -> ""
           | dir -> dir
         in
         let css = CodocUtil.(ascent_of_depth css (depth idx.path)) in
         render_index ~scheme ~css name idx path)
      parents
  end


module StringMap = Map.Make(String)

let read_interfaces ~force ~index ~env pkg_index out_dir =
  let open CodocIndex in
  let add_error err = function
    | `Error errs -> `Error (err :: errs)
    | `Ok _ -> `Error [err]
  in
  let add_errors errs = function
    | `Error errs -> `Error (errs @ errs)
    | `Ok _ -> `Error errs
  in
  let add_interface path intf = function
    | `Error _ as err -> err
    | `Ok map -> `Ok (StringMap.add path intf map)
  in
  let unit_f acc idx { xml_file; } =
    let html_file = html_name_of xml_file in
    let path =
      match Filename.dirname idx.path with
      |"." -> ""
      | p -> p
    in
    let xml_file = out_dir / path / xml_file in
    let path = out_dir / path / html_file in
    match read_unit xml_file with
    | `Error errs -> add_errors errs acc
    | `Ok unit ->
      match unit_interface ~force ~env ~unit_file:path unit with
      | `Error _ as err -> add_error err acc
      | `Ok intf ->
        if not index then add_interface path intf acc
        else begin
          match check_file ~force (out_dir / idx.path) with
          | Some err -> add_error err acc
          | None -> add_interface path intf acc
        end
  in
  fold_down_units ~unit_f (`Ok StringMap.empty) pkg_index

let render_interfaces ~index ~scheme ~css ~env pkg_index out_dir intfs =
  let open CodocIndex in
  let unit_f acc idx gunit =
    let path = match Filename.dirname idx.path with "." -> "" | p -> p in
    let html_file = html_name_of gunit.xml_file in
    let pkg_root = Some CodocUtil.(ascent_of_depth "" (depth html_file)) in
    let html_path = path / html_file in
    let css = CodocUtil.(ascent_of_depth css (depth html_path)) in
    let intf_file = out_dir / html_path in
    let intf = StringMap.find intf_file intfs in (* TODO: shouldn't exn... *)
    let html_files = render_interface ~scheme ~pkg_root ~css ~env intf in
    print_html_file_issues html_files;
    if index then begin
      let out_index = read_cache { idx with root = out_dir } idx.path in
      let html_files = Some html_files in
      let index = set_gunit out_index { gunit with html_files } in
      write_cache index
    end;
    acc
  in
  fold_down_units ~unit_f () pkg_index

let run_index ~force ~index in_index out_dir package scheme css =
  let root = Filename.dirname in_index in
  let path = Filename.basename in_index in
  let idx = CodocIndex.read root path in
  let parents = CodocIndex.diff idx package in
  match check_parents ~force ~index out_dir parents with
  | Some errs -> CodocCli.combine_errors errs
  | None ->
    let pkg_index = CodocIndex.goto idx package in
    match check_indices ~force ~index out_dir pkg_index with
    | Some errs -> CodocCli.combine_errors errs
    | None ->
      let env = CodocEnvironment.create idx in
      match read_interfaces ~force ~index ~env pkg_index out_dir with
      | `Error errs -> CodocCli.combine_errors errs
      | `Ok interfaces ->
        render_interfaces ~index ~scheme ~css ~env pkg_index out_dir interfaces;
        render_indices ~index ~scheme ~css out_dir pkg_index;
        render_parents ~index ~scheme ~css out_dir parents;
        CodocIndex.flush_cache idx;
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
  let out_dir = Filename.dirname out_file in
  let render_f = render_interface_ok ~force in_file out_file scheme in
  render_with_css ~force share out_dir render_f css

let run ({ CodocCli.Common.force; index }) output path package scheme css share =
  share_dir := share;
  match path, output with
  | `Missing path, _ -> Error.source_missing path
  | `File in_file, None -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface ->
        let html_file = html_name_of in_file in
        render_file ~force in_file html_file scheme css share
      | Index ->
        let out_dir = Filename.dirname in_file in
        let render_f =
          run_index ~force ~index in_file out_dir package scheme
        in
        render_with_css ~force share out_dir render_f css
    end
  | `File in_file, Some (`Missing out_file) -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface -> render_file ~force in_file out_file scheme css share
      | Index ->
        let render_f =
          run_index ~force ~index in_file out_file package scheme
        in
        render_with_css ~force share out_file render_f css
    end
  | `File in_file, Some (`File out_file) -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface -> render_file ~force in_file out_file scheme css share
      | Index -> Error.index_to_file in_file out_file
    end
  | `File in_file, Some (`Dir out_dir) -> begin
      match CodocSysUtil.deduce_file_type in_file with
      | Unknown -> Error.unknown_file_type in_file
      | Interface ->
        let base_name = Filename.basename in_file in
        let html_name = html_name_of base_name in
        render_file ~force in_file (out_dir / html_name) scheme css share
      | Index ->
        let render_f =
          run_index ~force ~index in_file out_dir package scheme
        in
        render_with_css ~force share out_dir render_f css
    end
  | `Dir in_dir, None -> begin
      match CodocSysUtil.search_for_source in_dir with
      | None -> Error.source_not_found in_dir
      | Some (source, Unknown) -> Error.unknown_file_type source
      | Some (source, Interface) ->
        let html_file = html_name_of source in
        render_file ~force source html_file scheme css share
      | Some (source, Index) ->
        let render_f =
          run_index ~force ~index source in_dir package scheme
        in
        render_with_css ~force share in_dir render_f css
    end
  | `Dir in_dir, Some (`Missing out_dir | `Dir out_dir) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      let base_name = Filename.basename source in
      let html_name = html_name_of base_name in
      render_file ~force source (out_dir / html_name) scheme css share
    | Some (source, Index) ->
      let render_f =
        run_index ~force ~index source out_dir package scheme
      in
      render_with_css ~force share out_dir render_f css
    end
  | `Dir in_dir, Some (`File out_file) ->
    begin match CodocSysUtil.search_for_source in_dir with
    | None -> Error.source_not_found in_dir
    | Some (source, Unknown) -> Error.unknown_file_type source
    | Some (source, Interface) ->
      render_file ~force source out_file scheme css share
    | Some (source, Index) -> Error.index_to_file source out_file
    end
