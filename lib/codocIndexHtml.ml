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

let of_issue = CodocIndex.(BlueTree.(function
  | Module_resolution_failed mod_name ->
    of_cons "module_resolution_failed" (of_cons "name" (of_string mod_name))
  | Xml_error (xml_file,(l,c),msg) ->
    let xml_loc = of_xml_location xml_file l c in
    of_cons "xml_error" (of_kv [
      "loc", xml_loc;
      "message", of_string msg;
    ])
  | Template_error message -> of_cons_string "template_error" message
))

let sort_issues = List.sort CodocIndex.(fun a b -> match a,b with
  | Module_resolution_failed x, Module_resolution_failed y -> compare x y
  | Xml_error (xml_file,pos,msg), Xml_error (xml_file',pos',msg') ->
    compare (xml_file,pos,msg) (xml_file',pos',msg')
  | Template_error m, Template_error m' -> compare m m'
  | Template_error _, _  ->  1
  | _, Template_error _  -> -1
  | Xml_error (_,_,_), _ ->  1
  | _, Xml_error (_,_,_) -> -1
)

let of_package ~name ~index ~normal_uri ~uri_of_path =
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
  (* TODO: remove repetition *)
  let units = List.map (function
    | (name, { CodocIndex.html_file = None; issues = [] }) ->
      BlueTree.of_kv_string ["name", name]
    | (name, { CodocIndex.html_file = None; issues }) ->
      BlueTree.(of_kv [
        "name", of_string name;
        "issues", of_list (List.map of_issue (sort_issues issues));
      ])
    | (name, { CodocIndex.html_file = Some html_file; issues = [] }) ->
      BlueTree.of_kv_string [
        "name", name;
        "href", Uri.to_string (uri_of_path html_file);
      ]
    | (name, { CodocIndex.html_file = Some html_file; issues }) ->
      BlueTree.(of_kv [
        "name", of_string name;
        "href", of_string (Uri.to_string (uri_of_path html_file));
        "issues", of_list (List.map of_issue (sort_issues issues));
      ])
  ) (List.sort compare units) in
  BlueTree.(of_kv_maybe ([
    "pkg-path", Some (of_list pkg_path);
    "pkgs", (match pkgs with [] -> None | pkgs -> Some (of_list pkgs));
    "modules", (match units with [] -> None | units -> Some (of_list units));
    "up", up;
  ]))
