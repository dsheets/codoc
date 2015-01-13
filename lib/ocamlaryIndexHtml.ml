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

let link_pkg_piece href piece = <:html<<a href=$uri:href$>$str:piece$</a>&>>

let rec link_pkg_pieces ~normal_uri href next = function
  | [] -> []
  | [last] -> [ <:html<$str:last$>> ]
  | h::t ->
    let next_href = normal_uri Uri.(resolve "" href (of_string next)) in
    (link_pkg_piece next_href next)::(link_pkg_pieces ~normal_uri next_href h t)

let root_name = "~"

let of_issue = OcamlaryIndex.(function
  | Module_resolution_failed mod_name ->
    <:html<<li>Module <strong>$str:mod_name$</strong> not found</li>&>>
  | Xml_error (xml_file,msg) ->
    <:html<
    <li>XML error parsing <strong>$str:xml_file$</strong>: $str:msg$</li>
    >>
)

let sort_issues = List.sort OcamlaryIndex.(fun a b -> match a,b with
  | Module_resolution_failed x, Module_resolution_failed y -> compare x y
  | Xml_error (xml_file,msg), Xml_error (xml_file',msg') ->
    compare (xml_file,msg) (xml_file',msg')
  | Xml_error (_,_), _ | _, Xml_error (_,_) -> -1
)

let of_package ~name ~index ~normal_uri ~uri_of_path =
  let up = if name = "" then None else Some <:html<<a href="../">Up</a>&>> in
  let pkg_path = match Stringext.split ~on:'/' name with
    | [] -> <:html<$str:root_name$>>
    | [last] -> <:html<<a href="../">$str:root_name$</a> / $str:last$>>
    | first::rest ->
      let ascent = OcamlaryUtil.ascent_of_depth "" (List.length rest + 1) in
      let root = Uri.of_string ascent in
      let first_href = normal_uri Uri.(resolve "" root (of_string first)) in
      let pieces = OcamlaryHtml.fold_html_str " / "
        (link_pkg_piece first_href first)
        (link_pkg_pieces ~normal_uri first_href first rest)
      in
      <:html<<a href=$uri:root$>$str:root_name$</a> / $pieces$>>
  in
  let pkgs = StringMap.fold (fun name pkg lst -> (name,pkg)::lst)
    index.OcamlaryIndex.pkgs []
  in
  let pkgs = List.map (fun (name, _pkg) ->
    let href = normal_uri Uri.(of_string (name ^ "/")) in
    <:html<<li><a href=$uri:href$>$str:name$</a></li>&>>
  ) (List.sort compare pkgs) in
  let units = StringMap.fold (fun name unit lst -> (name,unit)::lst)
    index.OcamlaryIndex.units []
  in
  let units = List.map (function
    | (name, { OcamlaryIndex.html_file = None; issues = [] }) ->
      <:html<<li>$str:name$</li>&>>
    | (name, { OcamlaryIndex.html_file = None; issues }) ->
      <:html<<li>
        <details>
          <summary>$str:name$</summary>
          <ul>$list:List.map of_issue (sort_issues issues)$</ul>
        </details>
      </li>&>>
    | (name, { OcamlaryIndex.html_file = Some html_file; issues = [] }) ->
      <:html<<li><a href=$uri:uri_of_path html_file$>$str:name$</a></li>&>>
    | (name, { OcamlaryIndex.html_file = Some html_file; issues }) ->
      <:html<<li>
        <details>
          <summary><a href=$uri:uri_of_path html_file$>$str:name$</a></summary>
          <ul>$list:List.map of_issue (sort_issues issues)$</ul>
        </details>
      </li>&>>
  ) (List.sort compare units) in
  let pkgs = match pkgs with [] -> <:html<&>>
    | pkgs -> <:html<
      <section>
        <h2>Subpackages</h2>
        <ul>
          $list:pkgs$
        </ul>
      </section>
    >> in
  let units = match units with [] -> <:html<&>>
    | units -> <:html<
      <section>
        <h2>Modules</h2>
        <ul>
          $list:units$
        </ul>
      </section>
    >> in
  <:html<
  <div class="ocamlary-doc">
    $opt:up$
    <div class="package-index">
      <h1>Package $pkg_path$</h1>
      $pkgs$
      $units$
    </div>
  </div>
  >>
