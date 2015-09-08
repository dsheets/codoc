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

module StringMap = Map.Make(String)
module BlueTree = Blueprint.Tree

let link_pkg_piece href piece =
  BlueTree.of_kv_string [
    "href", Uri.to_string href;
    "anchor", piece;
  ]

let rec link_pkg_pieces ~normal_uri href = function
  | [] -> []
  | [last] -> [BlueTree.of_kv_string [ "anchor", last ]]
  | h::t ->
    let next_href = normal_uri Uri.(resolve "" href (of_string (h^"/"))) in
    (link_pkg_piece next_href h)::(link_pkg_pieces ~normal_uri next_href t)

let root_name = "~"

let of_xml_location file l c =
  (* TODO: link *)
  BlueTree.of_kv_string [
    "file", file;
    "line", string_of_int l;
    "col", string_of_int c;
  ]

let of_doc_error = CodocAnalysis.(BlueTree.(function
  | Empty_section label -> of_cons_string "empty-section" label
  | Empty_tag tag -> of_cons_string "empty-tag" tag
  | Unlinked_ref name -> of_cons_string "unlinked-ref" name
  | Unlinked_path name -> of_cons_string "unlinked-path" name
  | Unlinked_frag name -> of_cons_string "unlinked-frag" name
  | Empty_code -> of_cons "empty-code" (empty ())
  | Bad_stop -> of_cons "bad-stop" (empty ())
  | Doc_error message -> of_cons_string "doc-error" message
))

let of_issue = CodocIndex.(BlueTree.(function
  | Template_error message -> of_cons_string "template-error" message
  | Doc_error doc_error -> of_cons "doc-error" (of_doc_error doc_error)
))

let of_unit_issue = CodocIndex.(BlueTree.(function
  | Module_resolution_failed mod_name ->
    of_cons "module-resolution-failed" (of_cons "name" (of_string mod_name))
  | Non_cmti_source file ->
    of_cons "non-cmti-source" (of_string (CodocExtraction.rel_path file))
  | Xml_error (xml_file,(l,c),msg) ->
    let xml_loc = of_xml_location xml_file l c in
    of_cons "xml-error" (of_kv [
      "loc", xml_loc;
      "message", of_string msg;
    ])
))

let sort_issues = List.sort CodocIndex.(fun a b -> match a,b with
  | Template_error m, Template_error m' -> compare m m'
  | Doc_error e, Doc_error e' -> compare e e'
  | Doc_error _, _       ->  1
  | _, Doc_error _       -> -1
)

let sort_unit_issues = List.sort CodocIndex.(fun a b -> match a,b with
  | Module_resolution_failed x, Module_resolution_failed y -> compare x y
  | Non_cmti_source e, Non_cmti_source e' -> compare e e'
  | Xml_error e, Xml_error e' -> compare e e'
  | Xml_error _, _ ->  1
  | _, Xml_error _ -> -1
  | Non_cmti_source _, _ ->  1
  | _, Non_cmti_source _ -> -1
)

let sort_names = List.sort CodocUnit.Substruct.(fun a b ->
  String.compare (string_of_name a) (string_of_name b)
)

let substruct_type typ =
  BlueTree.([ typ, Some (empty ()); "type", Some (of_string typ) ])

let find_sub subl name =
  try Some (List.find (fun sub ->
    CodocUnit.Substruct.to_name sub = name
  ) subl)
  with Not_found -> None

let children = CodocUnit.Substruct.(function
  | None | Some (Class _ | ClassType _) -> []
  | Some (Module (_,cs,_)) -> cs
  | Some (ModuleType (_,cs,_)) -> cs
)

let rec of_substruct ~uri_of_path ~normal_uri scheme base substruct name =
  let open BlueTree in
  let name, typ, children, sub =
    destruct_name ~uri_of_path ~normal_uri scheme base substruct name
  in
  let substruct = match substruct with
    | None -> None
    | Some sub ->
      let pkg_root = Some "." in
      let base = Some base in
      Some (CodocUnitHtml.of_substruct scheme ~pkg_root ~base sub)
  in
  of_kv_maybe (typ@[
    "name", Some (of_string name);
    "issues", (match sub.CodocIndex.issues with
      | [] -> None
      | _::_ ->
        Some (of_list (List.map of_issue (sort_issues sub.CodocIndex.issues))));
    "href", (match sub.CodocIndex.html_file with
      | Some html_file ->
        let path = Filename.concat base html_file in
        Some (of_string (Uri.to_string (normal_uri (uri_of_path path))))
      | None -> None);
    "children", Some (of_list children);
    "interface", substruct;
  ])

and destruct_name ~uri_of_path ~normal_uri scheme base substruct =
  CodocUnit.Substruct.(BlueTree.(function
    | ClassName (name, sub) -> name, substruct_type "class", [], sub
    | ClassTypeName (name, sub) ->
      name, substruct_type "classtype", [], sub
    | ModuleName (name, cs, sub) ->
      let recurse name =
        let sub = find_sub (children substruct) name in
        of_substruct ~uri_of_path ~normal_uri scheme base sub name
      in
      name,
      substruct_type "module",
      List.map recurse (sort_names cs),
      sub
    | ModuleTypeName (name, cs, sub) ->
      let recurse name =
        let sub = find_sub (children substruct) name in
        of_substruct ~uri_of_path ~normal_uri scheme base sub name
      in
      name,
      substruct_type "moduletype",
      List.map recurse (sort_names cs),
      sub
  ))

let of_package ~name ~index ~substructs ~scheme =
  let normal_uri = CodocUnit.Href.normal_uri_for_scheme scheme in
  let uri_of_path = CodocUnit.Href.uri_of_path ~scheme in
  let up =
    if name = ""
    then None
    else let href = Uri.to_string (normal_uri (Uri.of_string "../")) in
         Some (BlueTree.of_kv_string [ "href", href ])
  in
  let pkg_path = match Stringext.split ~on:'/' name with
    | [] -> link_pkg_pieces ~normal_uri Uri.empty [root_name]
    | [last] ->
      let href = normal_uri (Uri.of_string "../") in
      (link_pkg_piece href root_name)::(link_pkg_pieces ~normal_uri href [last])
    | first::rest ->
      let ascent = CodocUtil.ascent_of_depth "" (List.length rest + 1) in
      let root = normal_uri (Uri.of_string ascent) in
      let first_href =
        normal_uri Uri.(resolve "" root (of_string (first ^ "/")))
      in
      (link_pkg_piece root root_name)::
      (link_pkg_piece first_href first)::
      (link_pkg_pieces ~normal_uri first_href rest)
  in
  let pkgs = StringMap.fold (fun name pkg lst -> (name,pkg)::lst)
    index.CodocIndex.pkgs []
  in
  let pkgs = List.map (fun (name, _pkg) ->
    let href = normal_uri Uri.(of_string (name ^ "/")) in
    BlueTree.of_kv_string ["href", Uri.to_string href; "name", name]
  ) (List.sort compare pkgs) in
  let units = StringMap.fold (fun name unit lst -> (name,unit)::lst)
    index.CodocIndex.units []
  in
  let units = List.fold_left BlueTree.(CodocIndex.(fun list -> function
    | (_, { hide = true }) -> list
    | (name, { xml_file; substructs=subname; unit_issues }) ->
      let base = Filename.dirname xml_file in
      let maybe_sub = find_sub substructs subname in
      (of_kv [
        "name", of_string name;
        "module",
        of_substruct ~uri_of_path ~normal_uri scheme base maybe_sub subname;
        "issues",
        of_list (List.map of_unit_issue (sort_unit_issues unit_issues));
      ])::list)) [] (List.sort compare units) in
  let units = List.rev units in
  BlueTree.(of_kv_maybe ([
    "pkg-path", Some (of_list pkg_path);
    "pkgs", (match pkgs with [] -> None | pkgs -> Some (of_list pkgs));
    "units", (match units with [] -> None | units -> Some (of_list units));
    "up", up;
  ]))
