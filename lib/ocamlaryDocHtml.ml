(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open DocOckTypes
open DocOckPaths

open OcamlaryDocMaps
open OcamlaryDoc

type 'a href = 'a * Uri.t

type pathloc = {
  root        : root;
  path        : root Identifier.signature;
  index       : root -> Uri.t option;
}

let pathloc ~unit ~index =
  let signature = Identifier.signature_of_module unit.Unit.id in
  {
    root = fst (Maps.root_of_ident_signature signature);
    path = signature;
    index;
  }

let self_uri = Uri.of_string ""

let keyword text = <:html<<span class="keyword">$str:text$</span>&>>

let type_class = "type"
let exn_class = "exn" (* exception *)

(* TODO: use these for CSS class names? *)
let ident_class = Identifier.(function
  | Root _ -> "root"
  | Module _ -> "module"
  | Argument _ -> "modarg"
  | ModuleType _ -> "modtype"
  | Type _ -> type_class
  | CoreType _ -> type_class
  | Constructor _ -> "cons" (* const *)
  | Field _ -> "field" (* recfield *)
  | Extension _ -> "ext"
  | Exception _ -> exn_class
  | CoreException _ -> exn_class
  | Value _ -> "val"
  | Class _ -> "class"
  | ClassType _ -> "classtype"
  | Method _ -> "method"
  | InstanceVariable _ -> "var" (* attribute *)
  | Label _ -> "label" (* section *)
)

let classify cls name = Printf.sprintf "%s:%s" cls name

let name_of_argument i name = Printf.sprintf "%d:%s" i name

class id_of_ident_map ~pathloc :
  [unit, root, string] Identifier.any_fold =
object (self)
  inherit [unit, root, string] Identifier.any_parent_fold

  method root () _ _ = ""
  method core_type () name = classify type_class name
  method core_exception () name = classify exn_class name
  method argument () parent i name =
    self#parent ()
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      (name_of_argument i name)
  method private parent () ident parent name =
    Printf.sprintf "%s/%s"
      (Identifier.fold_any self () (Identifier.any parent))
      (classify (ident_class ident) name)
end

let id_of_ident_map ~pathloc = new id_of_ident_map ~pathloc

let id_of_ident ~pathloc = Identifier.fold_any (id_of_ident_map ~pathloc) ()

class href_of_ident_map ~pathloc :
  [unit, root, Uri.t option] Identifier.any_fold =
object (self)
  inherit [unit, root, Uri.t option] Identifier.any_parent_fold

  method root () r _name =
    if r = pathloc.root
    then Some self_uri
    else pathloc.index r
  method core_type () name = None (* TODO: fixme *)
  method core_exception () name = None (* TODO: fixme *)
  method argument () parent i name =
    self#parent ()
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      (name_of_argument i name)
  method private parent () ident parent name = (* TODO: projections *)
    match Maps.root_of_ident (Identifier.any parent) with
    | None -> None
    | Some (root, name) -> match self#root () root name with
      | None -> None
      | Some base ->
        Some Uri.(with_fragment base (Some (id_of_ident ~pathloc ident)))
end

let href_of_ident_map ~pathloc = new href_of_ident_map ~pathloc

let href_of_ident ~pathloc : 'a Identifier.any -> Uri.t option =
  Identifier.fold_any (href_of_ident_map ~pathloc) ()

class link_ident_map ?text ~pathloc () :
  [unit, root, Cow.Html.t] Identifier.any_fold =
object (self)
  inherit [unit, root, Cow.Html.t] Identifier.any_parent_fold

  method root () r name =
    match (new href_of_ident_map ~pathloc)#root () r name with
    | None -> <:html<$str:name$>>
    | Some uri -> <:html<<a href=$uri:uri$>$str:name$</a>&>>
  method core_type () name = <:html<$str:name$>> (* TODO: link *)
  method core_exception () name = <:html<$str:name$>> (* TODO: link *)
  method argument () parent i name =
    self#parent ()
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      (name_of_argument i name)
  method private parent () ident parent name =
    match href_of_ident ~pathloc ident, text with
    | None, None ->
      let phtml = Identifier.fold_any self () (Identifier.any parent) in
      <:html<$phtml$.$str:name$>>
    | Some href, None ->
      let phtml = Identifier.fold_any self () (Identifier.any parent) in
      <:html<$phtml$.<a href=$uri:href$>$str:name$</a>&>>
    | None, Some html -> html
    | Some href, Some html -> <:html<<a href=$uri:href$>$html$</a>&>>
end

let link_ident_map ?text ~pathloc () =
  new link_ident_map ?text ~pathloc ()

let link_ident ?text ~pathloc () =
  Identifier.fold_any (link_ident_map ?text ~pathloc ()) ()

class link_resolved_path_map ~pathloc link_path
  : [unit, root, Cow.Html.t, Path.kind] Path_resolved.any_fold =
object (self)
  inherit [unit, root, Cow.Html.t] Path_resolved.any_parent_fold

  method identifier () ident =
    let ident = Identifier.any ident in
    let text = <:html<$str:Identifier.name ident$>> in
    link_ident ~text ~pathloc () ident
  method subst () _rsub li =
    Path_resolved.(fold_any self () (any li))
  method subst_alias () _rsub li =
    Path_resolved.(fold_any self () (any li))
  method apply () func module_path =
    let html = Path_resolved.(fold_any self () (any func)) in
    <:html<$html$($link_path ~pathloc (Path.any module_path)$)>>
  method private parent () path parent _name =
    let phtml = Path_resolved.(fold_any self () (any parent)) in
    let link = self#identifier () (Path.Resolved.identifier path) in
    <:html<$phtml$.$link$&>>
end

let link_resolved_path_map ~pathloc link_path =
  new link_resolved_path_map ~pathloc link_path

let link_resolved_path ~pathloc link_path =
  Path_resolved.fold_any (link_resolved_path_map ~pathloc link_path) ()

let rec link_path ~pathloc : ('a,'b) Path.t -> Cow.Html.t =
  Path.(function
  | Root root -> <:html<$str:root$>>
  | Dot (parent, name) ->
    <:html<$link_path ~pathloc (any parent)$.$str:name$>>
  | Resolved resolved -> link_resolved_path ~pathloc link_path resolved
  | Apply (m, m') -> (* TODO: test *)
    <:html<$link_path ~pathloc (any m)$($link_path ~pathloc (any m')$)>>
  )

class link_resolved_fragment_map ~pathloc base
  : [unit, root, Cow.Html.t, Fragment.Resolved.kind, [`Branch]]
  Fragment_resolved.any_fold =
object (self)
  inherit [unit, root, Cow.Html.t, Fragment.Resolved.kind, [`Branch]]
    Fragment_resolved.any_parent_fold

  method subst () _rsub li = Fragment_resolved.fold_any self () li
  method subst_alias () _rsub li = Fragment_resolved.fold_any self () li

  method private parent () fragment parent name = Fragment_resolved.(
    let text = <:html<$str:name$>> in
    let link = match base with
      | Some base ->
        let ident = Identifier.any (identifier base fragment) in
        link_ident ~text ~pathloc () ident
      | None -> text
    in
    let rec render : root signature -> Cow.Html.t = function
      | Root -> link
      | Subst (_, li) -> render li
      | SubstAlias (_, li) -> render li
      | Module _ as parent -> (* need to capture GADT eq *)
        let phtml = fold_any self () parent in
        <:html<$phtml$.$link$>>
    in render parent
  )
end

let link_resolved_fragment_map ~pathloc base =
  new link_resolved_fragment_map ~pathloc base

let link_resolved_fragment ~pathloc base =
  Fragment_resolved.fold_any (link_resolved_fragment_map ~pathloc base) ()

let rec link_fragment ~pathloc base
    : 'a Fragment.any -> Cow.Html.t =
  Fragment.(function
  | Resolved resolved -> link_resolved_fragment ~pathloc base resolved
  | Dot (Resolved Resolved.Root, name) -> <:html<$str:name$>> (* TODO: test *)
  | Dot ((Dot _ | Resolved (Resolved.Module _)) as parent, name) ->
    <:html<$link_fragment ~pathloc base (any parent)$.$str:name$>>
  | signature -> match split signature with (* TODO: test *)
    | name, None -> <:html<$str:name$>>
    | name, Some parent ->
      <:html<$link_fragment ~pathloc base parent$.$str:name$>>
  )

class link_resolved_reference_map ?text ~pathloc ()
  : [unit, root, Cow.Html.t, Reference.kind] Reference_resolved.any_fold =
object (self)
  inherit [unit, root, Cow.Html.t] Reference_resolved.any_parent_fold

  method identifier () ident =
    let ident = Identifier.any ident in
    let text = match text with
      | None -> <:html<$str:Identifier.name ident$>>
      | Some text -> text
    in
    link_ident ~text ~pathloc () ident
  method private parent () reference parent _name =
    let link = self#identifier () (Reference.Resolved.identifier reference) in
    match text with
    | None ->
      let phtml = Reference_resolved.(fold_any self () (any parent)) in
      <:html<$phtml$.$link$&>>
    | Some _ -> link
end

let link_resolved_reference_map ?text ~pathloc () =
  new link_resolved_reference_map ?text ~pathloc ()

let link_resolved_reference ?text ~pathloc () =
  Reference_resolved.fold_any (link_resolved_reference_map ?text ~pathloc ()) ()

let rec link_reference ?text ~pathloc : ('a,'b) Reference.t -> Cow.Html.t =
  Reference.(function
  | Root root -> <:html<[root:$str:root$]>> (* TODO: test wtf *)
  | Dot (parent, name) ->
    begin match text with
    | None -> <:html<$link_reference ~pathloc (any parent)$.$str:name$>>
    | Some html -> html (* TODO: link ?? *)
    end
  | Resolved resolved -> link_resolved_reference ?text ~pathloc () resolved
  )

class string_of_resolved_reference_map
  : [unit, root, string, Reference.kind] Reference_resolved.any_fold =
object (self)
  inherit [unit, root, string] Reference_resolved.any_parent_fold

  method identifier () ident = Maps.string_of_ident (Identifier.any ident)
  method private parent () _reference parent name =
    Reference_resolved.(fold_any self () (any parent))^"."^name
end

let string_of_resolved_reference_map = new string_of_resolved_reference_map

let string_of_resolved_reference =
  Reference_resolved.fold_any string_of_resolved_reference_map ()

let rec string_of_reference : ('a,'b) Reference.t -> string =
  Reference.(function
  | Root root -> root
  | Dot (parent, name) -> (string_of_reference (any parent))^"."^name
  | Resolved resolved -> string_of_resolved_reference resolved
  )

let anchor ~pathloc id html =
  let a = match href_of_ident ~pathloc id with
    | Some href -> <:html<<a href=$uri:href$ class="anchor">#</a>&>>
    | None -> <:html<&>>
  in
  <:html<
  <div class="region" id=$str:id_of_ident ~pathloc id$>
  $a$
  $html$
  </div>
  >>

let section_attrs ~pathloc level =
  ["class","section level_"^(string_of_int level)]

(* TODO: ids for unnamed sections? ocamldoc does it... *)
let label_anchor ~pathloc label_opt html = match label_opt with
  | None -> <:html<<div class="region">$html$</div>&>>
  | Some label ->
    anchor ~pathloc (Identifier.any label) html

let rec of_text_element ~pathloc txt =
  let of_text_elements = of_text_elements ~pathloc in
  Reference.(Documentation.(match txt with
  | Raw s -> <:html<$str:s$>>
  | Code "" -> <:html<&>> (* TODO: warn this isn't the empty list literal *)
  | Code s -> <:html<<code>$str:s$</code>&>>
  | PreCode s -> <:html<<pre><code>$str:s$</code></pre>&>>
  | Verbatim s -> <:html<<pre>$str:s$</pre>&>>
  | Target (None,href) ->
    <:html<<a href=$str:href$>TARGET NONE: $str:href$</a>&>> (* TODO: test *)
  | Target (Some a,href) ->
    <:html<<a href=$str:href$>TARGET SOME: $str:a$</a>&>> (* TODO: test *)
  | Reference (Link href, None) ->
    <:html<<a href=$str:href$>REF_LINK NONE: $str:href$</a>&>> (* TODO: test *)
  | Reference (Link href, Some t) -> (* hyperlink *)
    <:html<<a href=$str:href$>$of_text_elements t$</a>&>>
  | Style (Bold, els) ->
    <:html<<b>$of_text_elements els$</b>&>>
  | Style (Italic, els) ->
    <:html<<i>$of_text_elements els$</i>&>>
  | Style (Emphasize, els) ->
    <:html<<em>$of_text_elements els$</em>&>>
  | Style (Center, els) ->
    <:html<<div class="doc-center">$paragraphs_of_text ~pathloc els$</div>&>>
  | Style (Left, els) ->
    <:html<<div class="doc-left">$paragraphs_of_text ~pathloc els$</div>&>>
  | Style (Right, els) ->
    <:html<<div class="doc-right">$paragraphs_of_text ~pathloc els$</div>&>>
  | Style (Superscript, els) ->
    <:html<<sup>$of_text_elements els$</sup>&>>
  | Style (Subscript, els) ->
    <:html<<sub>$of_text_elements els$</sub>&>>
  | Style (Custom s, els) ->
    <:html<
    <span style="color:orange">CUSTOM($str:s$) $of_text_elements els$</span>
    >> (* TODO: test *)
  | List elss ->
    <:html<<ul>$list:lis_of_elss ~pathloc elss$</ul>&>>
  | Enum elss ->
    <:html<<ol>$list:lis_of_elss ~pathloc elss$</ol>&>>
  | Newline ->
    <:html<<br />&>>
  | Title (1 as level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h1 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h1>
    >>
  | Title (2 as level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h2 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h2>
    >>
  | Title (3 as level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h3 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h3>
    >>
  | Title (4 as level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h4 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h4>
    >>
  | Title (5 as level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h5 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h5>
    >>
  | Title (6 as level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h6 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h6>
    >>
  | Title (level,label_opt,els) when level < 1 ->
    label_anchor ~pathloc label_opt
    <:html<
    <h1 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h1>
    >>
  | Title (level,label_opt,els) ->
    label_anchor ~pathloc label_opt
    <:html<
    <h6 $alist:section_attrs ~pathloc level$>$of_text_elements els$</h6>
    >>
  | Reference (Module m, None) -> link_reference ~pathloc (any m)
  | Reference (Module m, Some els) ->
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any m)
  | Reference (ModuleType m, None) -> link_reference ~pathloc (any m)
  | Reference (ModuleType m, Some els) ->
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any m)
  | Reference (Type t, None) -> link_reference ~pathloc (any t)
  | Reference (Type t, Some els) ->
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any t)
  | Reference (Constructor c, None) -> link_reference ~pathloc (any c)
  | Reference (Constructor c, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any c)
  | Reference (Field f, None) -> link_reference ~pathloc (any f)
  | Reference (Field f, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any f)
  | Reference (Extension e, None) -> link_reference ~pathloc (any e)
  | Reference (Extension e, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any e)
  | Reference (Exception e, None) -> link_reference ~pathloc (any e)
  | Reference (Exception e, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any e)
  | Reference (Value v, None) -> link_reference ~pathloc (any v)
  | Reference (Value v, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any v)
  | Reference (Class c, None) -> link_reference ~pathloc (any c)
  | Reference (Class c, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any c)
  | Reference (ClassType c, None) -> link_reference ~pathloc (any c)
  | Reference (ClassType c, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any c)
  | Reference (Method m, None) -> link_reference ~pathloc (any m)
  | Reference (Method m, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any m)
  | Reference (InstanceVariable i, None) -> link_reference ~pathloc (any i)
  | Reference (InstanceVariable i, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any i)
  | Reference (Element e, None) -> link_reference ~pathloc (any e)
  | Reference (Element e, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any e)
  | Reference (Section s, None) -> link_reference ~pathloc (any s)
  | Reference (Section s, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any s)
  | Reference (Custom (s,s'), None) -> (* TODO: test *)
    <:html<<span style="color: red">CUSTOM: $str:s$ $str:s'$</span>&>>
  | Reference (Custom (s,s'), Some els) -> (* TODO: test *)
    <:html<<span style="color: red">
           CUSTOM: $of_text_elements els$ $str:s$ $str:s'$
           </span>
    >>
  | Special (Modules mods) ->
    let mods_html = List.map (fun m ->
      <:html<<tr><td>$link_reference ~pathloc (any m)$</td></tr>&>>
    ) mods in
    <:html<
    <table class="module_index">
    $list:mods_html$
    </table>
    >>
  | Special Index -> (* TODO: test *)
    <:html<<span style="color: red">
           Special index
           </span>
    >>
  ))
and lis_of_elss ~pathloc elss = List.map (fun els ->
  <:html<<li>$of_text_elements ~pathloc els$</li>&>>
) elss
and of_text_elements ~pathloc els =
  let f = of_text_element ~pathloc in
  <:html<$list:List.map f els$>>
and paragraphs_of_text ~pathloc els =
  let paras = List.map (function
    | Para els ->
      <:html<
      <p>$of_text_elements ~pathloc els$</p>
      >>
    | Block els ->
      <:html<$of_text_elements ~pathloc els$>>
  ) (paragraphize els) in
  <:html<$list:paras$>>

let div_tag classes label html =
  let classes = "tag "^classes in
  <:html<
  <div class=$str:classes$>
    <span class="label">$str:label$</span> $html$
  </div>
  >>

let span_tag classes label html =
  let classes = "tag "^classes in
  <:html<
  <span class=$str:classes$>
    <span class="label">$str:label$</span> $html$
  </span>
  >>

let map_tag ~pathloc tag_fun tag =
  let of_text_elements = of_text_elements ~pathloc in
  Documentation.(match tag with
  | Author s ->
    tag_fun "author" "Author" <:html<$str:s$>>
  | Version s ->
    tag_fun "version" "Version" <:html<$str:s$>>
  | See (Url url_s, t) ->
    tag_fun "see-also" "See also"
      (of_text_elements
         ((Reference (Link url_s, Some [Raw url_s])) :: Raw " " :: t))
  | See (File file_s, t) -> (* TODO: test *)
    tag_fun "see-also" ("See also FILE "^file_s) (of_text_elements t)
  | See (Doc doc_s, t) -> (* TODO: test *)
    tag_fun "see-also" ("See also DOC "^doc_s) (of_text_elements t)
  | Since s ->
    tag_fun "since" "Since" <:html<$str:s$>>
  | Before (s,t) ->
    tag_fun "before" ("Before "^s) (of_text_elements t)
  | Deprecated t ->
    tag_fun "deprecated" "Deprecated" (of_text_elements t)
  | Param (s,t) ->
    tag_fun "param" s (of_text_elements t)
  | Raise (s,t) ->
    tag_fun "raises" "Raises"
    <:html<<code>$str:s$</code> $of_text_elements t$>>
  | Return t ->
    tag_fun "return" "Returns" (of_text_elements t)
  | Tag (s, t) ->
    tag_fun ("custom "^s) (String.capitalize s) (of_text_elements t)
  )

let maybe_div_doc ~pathloc ({ Documentation.text; tags }) =
  match text, tags with
  | [], [] -> <:html<&>>
  | text, tags ->
    <:html<
      <div class="doc">
        $paragraphs_of_text ~pathloc text$
        $list:List.map (map_tag ~pathloc div_tag) tags$
      </div>
    >>

let maybe_td_doc ~pathloc doc = Documentation.(match doc.text with
  | [] -> <:html<<td/><td/><td/>&>>
  | text ->
    let tags = List.map (map_tag ~pathloc span_tag) doc.tags in
    let comment = of_text_elements ~pathloc text in
    <:html<<td>(*</td><td class="doc">$comment$$list:tags$</td><td>*)</td>&>>
)

let fold_html sep = List.fold_left (fun phtml ehtml ->
  <:html<$phtml$$sep$$ehtml$>>
)

let fold_html_str sep = fold_html <:html<$str:sep$>>

let rec of_type_expr ?(group=false) ~pathloc expr =
  let of_type_expr = of_type_expr ~pathloc in
  TypeExpr.(match expr with
  | Var v when v = "_" -> <:html<_>>
  | Var v -> <:html<'$str:v$>>
  | Any -> <:html<_>>
  | Alias (t,v) -> (* TODO: parens are only sometimes required *)
    <:html<($of_type_expr t$ $keyword "as"$ '$str:v$)&>>
  | Arrow (label, (Arrow _ as t), t') ->
      (* Tuple binds more tightly *)
    let domain = of_labeled_type_expr ~group:true ~pathloc t label in
    let html = <:html<$domain$ &rarr; $of_type_expr t'$>> in
    if group then <:html<($html$)>> else html
  | Arrow (label, t, t') ->
    let domain = of_labeled_type_expr ~pathloc t label in
    let html = <:html<$domain$ &rarr; $of_type_expr t'$>> in
    if group then <:html<($html$)>> else html
  | Tuple []       -> <:html<()>>
  | Tuple (e::els) ->
    let e = of_type_expr ~group:true e in
    let els = List.map (of_type_expr ~group:true) els in
    let html = fold_html_str " * " e els in
    if group then <:html<($html$)>> else html
  | Constr (path, argl) -> of_type_constr ~pathloc path argl
  | Variant { Variant.kind=Variant.Fixed; elements } ->
    <:html<[ $list:polyvar_elements ~pathloc elements$ ]&>>
  | Variant { Variant.kind=Variant.Closed csl; elements } -> (* TODO: test csl *)
    let els = polyvar_elements ~pathloc elements in
    let open_tags = match csl with
      | []  -> <:html<&>>
      | csl ->
        let clopen = String.concat " " (List.map (fun x -> "`"^x) csl) in
        <:html< &gt; $str:clopen$&>>
    in
    <:html<[&lt; $list:els$$open_tags$ ]&>>
  | Variant { Variant.kind=Variant.Open; elements } ->
    <:html<[&gt; $list:polyvar_elements ~pathloc elements$ ]&>>
  | Object { Object.methods=[]; open_ } ->
    <:html<&lt; $str:if open_ then ".." else ""$ &gt;>>
  | Object { Object.methods=meth::meths; open_ } ->
    let mhtml = fold_html_str "; "
      (of_object_method ~pathloc meth)
      (List.map (of_object_method ~pathloc) meths)
    in
    let rest = if open_ then "; .." else "" in
    <:html<&lt; $mhtml$$str:rest$ &gt;>>
  | Poly ([], expr) -> of_type_expr expr
  | Poly (vars, expr) ->
    let vars = String.concat " " (List.map (fun v -> "'"^v) vars) in
    <:html<$str:vars$ . $of_type_expr expr$&>>
  | Class (path, argl) ->
    of_type_constr ~cons:"#" ~pathloc (Path.type_of_class_type path) argl
  | Package { Package.path; substitutions=[] } ->
    <:html<($keyword "module"$ $link_path ~pathloc (Path.any path)$)>>
  | Package { Package.path; substitutions=sub::subs } ->
    let sub = of_package_sub ~pathloc path sub in
    let subs = List.map (of_package_sub ~pathloc path) subs in
    let subs = fold_html <:html< $keyword "and"$ >>
      <:html< $keyword "with"$ $sub$>> subs
    in
    <:html<($keyword "module"$ $link_path ~pathloc (Path.any path)$$subs$)>>
  )
and of_object_method ~pathloc { TypeExpr.Object.name; type_ } =
  <:html<$str:name$ : $of_type_expr ~pathloc type_$&>>
and of_type_constr ?(cons="") ~pathloc path = function
  | []  -> link_path ~pathloc (Path.any path)
  | [a] ->
    let arg = of_type_expr ~group:true ~pathloc a in
    <:html<$arg$ $str:cons$$link_path ~pathloc (Path.any path)$>>
  | a::argl ->
    let a = of_type_expr ~pathloc a in
    let argl = List.map (of_type_expr ~pathloc) argl in
    let args = fold_html_str ", " a argl in
    <:html<($args$) $link_path ~pathloc (Path.any path)$>>
and polyvar_element ~pathloc : 'a TypeExpr.Variant.element -> Cow.Html.t =
  TypeExpr.Variant.(function
  | Type expr -> of_type_expr ~pathloc expr
  | Constructor (name, _, []) -> <:html<`$str:name$>>
  | Constructor (name, empty, arg::args) ->
    let empty = if empty then <:html<&amp; >> else <:html<&>> in
    let args = List.map (of_type_expr ~pathloc) args in
    let arghtml = fold_html_str " & "
      <:html<$empty$$of_type_expr ~pathloc arg$>>
      args
    in
    <:html<`$str:name$ $keyword "of"$ $arghtml$&>>
  )
and polyvar_elements ~pathloc = List.map (fun pve ->
  <:html<<div class="cons">| $polyvar_element ~pathloc pve$</div>&>>
)
and of_labeled_type_expr ?(group=false) ~pathloc t = TypeExpr.(function
  | None -> of_type_expr ~group ~pathloc t
  | Some (Label l) -> <:html<$str:l$:$of_type_expr ~group ~pathloc t$>>
  | Some (Optional l) -> <:html<?$str:l$:$of_type_expr ~group ~pathloc t$>>
)
and of_package_sub ~pathloc path (fragment, expr) =
  let path = match path with
    | Path.Resolved resolved ->
      Some (Identifier.signature_of_module_type
              (Path.Resolved.identifier resolved))
    | _ -> None
  in
  let type_ = keyword "type" in
  let fragment_link = link_fragment ~pathloc path (Fragment.any fragment) in
  <:html<$type_$ $fragment_link$ = $of_type_expr ~pathloc expr$>>

let of_value ~pathloc { Value.id; doc; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let ident = Identifier.any id in
  let name = Identifier.name ident in
  anchor ~pathloc ident
  <:html<
  <div class="val">
    $keyword "val"$ $str:name$ : $of_type_expr ~pathloc type_$
    $doc$
  </div>
  >>

let of_external ~pathloc { External.id; doc; type_; primitives } =
  let doc = maybe_div_doc ~pathloc doc in
  let ident = Identifier.any id in
  let name = Identifier.name ident in
  let primitives = List.map (fun p -> <:html<"$str:p$" >>) primitives in
  anchor ~pathloc ident
  <:html<
  <div class="external val">
    $keyword "external"$ $str:name$ : $of_type_expr ~pathloc type_$
    = $list:primitives$
    $doc$
  </div>
  >>

let args_of_constructor ~pathloc args res =
  let of_type_expr = of_type_expr ~pathloc in
  match args, res with
  | [],       None    -> <:html<&>>
  | [TypeExpr.Tuple _ as a], None ->
    <:html< $keyword "of"$ ($of_type_expr a$)>>
  | [TypeExpr.Arrow _ as a], None ->
    <:html< $keyword "of"$ $of_type_expr a$>>
  | a::args,  None    ->
    let a = of_type_expr ~group:true a in
    let args = List.map (of_type_expr ~group:true) args in
    let arghtml = fold_html_str " * " a args in
    <:html< $keyword "of"$ $arghtml$>>
  | [],       Some rt -> <:html< : $of_type_expr rt$>>
  | a::args,  Some rt ->
    let a = of_type_expr ~group:true a in
    let args = List.map (of_type_expr ~group:true) args in
    let arghtml = fold_html_str " * " a args in
    <:html< : $arghtml$ &rarr; $of_type_expr rt$>>

let of_constructor ~pathloc { TypeDecl.Constructor.id; doc; args; res } =
  let id   = Identifier.any id in
  let name = Identifier.name id in
  let sig_ = args_of_constructor ~pathloc args res in
  let doc  = maybe_td_doc ~pathloc doc in
  let cons =
  anchor ~pathloc id
    <:html<| $str:name$$sig_$>>
  in
  <:html<<tr class="cons"><td>$cons$</td>$doc$</tr>&>>

let of_extension ~pathloc { Extension.Constructor.id; doc; args; res } =
  let id   = Identifier.any id in
  let name = Identifier.name (Identifier.any id) in
  let sig_ = args_of_constructor ~pathloc args res in
  let doc  = maybe_td_doc ~pathloc doc in
  let cons =
    anchor ~pathloc id
      <:html<| $str:name$$sig_$>>
  in
  <:html<<tr class="cons"><td>$cons$</td>$doc$</tr>&>>

let of_field ~pathloc { TypeDecl.Field.id; doc; type_; mutable_ } =
  let id = Identifier.any id in
  let name = Identifier.name id in
  let doc = maybe_td_doc ~pathloc doc in
  let thtml = of_type_expr ~pathloc type_ in
  let mutable_ =
    if mutable_ then <:html<$keyword "mutable"$ >> else <:html<&>>
  in
  let field = anchor ~pathloc id <:html<$mutable_$$str:name$>> in
  <:html<<tr class="field">
  <td>$field$</td><td>: $thtml$</td><td>;</td>$doc$
  </tr>&>>

let of_type_representation ~pathloc = TypeDecl.Representation.(function
  | Variant constrs ->
    <:html<<table>
    $list:List.map (of_constructor ~pathloc) constrs$
    </table>&>>
  | Record fields ->
    <:html<{<table>
    $list:List.map (of_field ~pathloc) fields$
    </table>}>>
  | Extensible -> <:html<..>>
)

let string_of_variance = TypeDecl.(function
  | None     -> ""
  | Some Pos -> "+"
  | Some Neg -> "-"
)

let of_type_params = TypeDecl.(function
    | []            -> <:html<&>>
    | [Any, vari]   ->
      <:html<$str:string_of_variance vari$_ >>
    | [Var p, vari] -> <:html<$str:string_of_variance vari$'$str:p$ >>
    | ps            ->
      let type_vars = List.map (function
        | Any,   vari -> string_of_variance vari ^ "_"
        | Var v, vari -> string_of_variance vari ^ "'"^v
      ) ps in
      <:html<($str:String.concat ", " type_vars$) >>
)

let of_type ~pathloc
    { TypeDecl.id; doc;
      equation={ TypeDecl.Equation.params; private_; manifest; constraints };
      representation;
    } =
  let of_type_expr = of_type_expr ~pathloc in
  let of_rep = of_type_representation ~pathloc in
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let params = of_type_params params in
  let priv = if private_ then <:html<$keyword "private"$ >> else <:html<&>> in
  let constraints = fold_html_str "" <:html<&>>
    (List.map (fun (c,c') ->
      <:html< $keyword "constraint"$ $of_type_expr c$ = $of_type_expr c'$>>
     ) constraints)
  in
  let rhs = match manifest, representation with
    | None, None -> <:html<&>>
    | Some t, None -> <:html< = $priv$$of_type_expr t$>>
    | None, Some r -> <:html< = $priv$$of_rep r$>>
    | Some t, Some r ->
      <:html< = $of_type_expr t$ = $priv$$of_rep r$>>
  in
  anchor ~pathloc id
  <:html<
  <div class="type">
    $keyword "type"$ $params$$str:name$$rhs$$constraints$
    $doc$
  </div>
  >>

let of_type_ext ~pathloc
    { Extension.type_path; doc; type_params; private_; constructors } =
  let doc = maybe_div_doc ~pathloc doc in
  let name_link = link_path ~pathloc (Path.any type_path) in
  let params = of_type_params type_params in
  let private_ = if private_ then keyword "private" else <:html<&>> in
  let constrs = List.map (of_extension ~pathloc) constructors in
  <:html<
  <div class="ext">
    $keyword "type"$ $params$$name_link$ += $private_$
    <table>
    $list:constrs$
    </table>
    $doc$
  </div>
  >>

let of_exception ~pathloc { Exception.id; doc; args; res } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let args = args_of_constructor ~pathloc args res in
  anchor ~pathloc id
  <:html<
  <div class="exn">
    $keyword "exception"$ $str:name$$args$
    $doc$
  </div>
  >>

let of_instance_variable ~pathloc
    { InstanceVariable.id; doc; mutable_; virtual_; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let mutable_ =
    if mutable_ then <:html< $keyword "mutable"$>> else <:html<&>>
  in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let type_ = of_type_expr ~pathloc type_ in
  anchor ~pathloc id
  <:html<
  <div class="classval">
  $keyword "val"$$mutable_$$virtual_$ $str:name$ : $type_$
  $doc$
  </div>
  >>

let of_method ~pathloc { Method.id; doc; private_; virtual_; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let private_ =
    if private_ then <:html< $keyword "private"$>> else <:html<&>>
  in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let type_ = of_type_expr ~pathloc type_ in
  anchor ~pathloc id
  <:html<
  <div class="method">
  $keyword "method"$$private_$$virtual_$ $str:name$ : $type_$
  $doc$
  </div>
  >>

let of_class_params = TypeDecl.(function
    | []            -> <:html<&>>
    | ps            ->
      let type_vars = List.map (function
        | Any,   vari -> string_of_variance vari ^ "_"
        | Var v, vari -> string_of_variance vari ^ "'"^v
      ) ps in
      <:html< [$str:String.concat ", " type_vars$]>>
)

let rec fold_doc_items f ?(suppress=false) acc = function
  | [] -> List.rev acc
  | next::rest -> match f next with
    | None -> let suppress = not suppress in fold_doc_items f ~suppress acc rest
    | Some _ when suppress -> fold_doc_items f ~suppress acc rest
    | Some html -> fold_doc_items f ~suppress (html::acc) rest

let rec of_class_signature_item ~pathloc = ClassSignature.(function
  | InstanceVariable instance -> Some (of_instance_variable ~pathloc instance)
  | Method method_ -> Some (of_method ~pathloc method_)
  | Constraint (a, b) ->
    Some <:html<
    <div class="constraint">
    $keyword "constraint"$ $of_type_expr ~pathloc a$ $of_type_expr ~pathloc b$
    </div>
    >>
  | Inherit class_type ->
    Some <:html<
    <div class="inherit">
    $keyword "inherit"$ $of_class_type_expr ~pathloc class_type$
    </div>
    >>
  | Comment (Documentation.Documentation doc) ->
    Some (maybe_div_doc ~pathloc doc)
  | Comment Documentation.Stop -> None
)
and of_class_type_expr ~pathloc = ClassType.(function
  | Constr (path, [])   -> link_path ~pathloc (Path.any path)
  | Constr (path, arg::args) ->
    let arg_html = of_type_expr ~pathloc arg in
    let args_html = List.map (of_type_expr ~pathloc) args in
    let args_html = fold_html_str ", " arg_html args_html in
    <:html<[$args_html$] $link_path ~pathloc (Path.any path)$>>
  | Signature { ClassSignature.self; items } ->
    let self = match self with
      | None -> <:html<&>>
      | Some expr -> <:html<($of_type_expr ~pathloc expr$) >>
    in
    let html_list =
      fold_doc_items (of_class_signature_item ~pathloc) [] items
    in
    <:html<$self$$list:html_list$>>
)

let rec of_class_decl ~pathloc = Class.(function
  | ClassType class_type_expr -> of_class_type_expr ~pathloc class_type_expr
  | Arrow (label, type_, decl) ->
    <:html<
    $of_labeled_type_expr ~pathloc type_ label$
    &rarr;
    $of_class_decl ~pathloc decl$
    >>
)

let of_class ~pathloc { Class.id; doc; virtual_; params; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let params = of_class_params params in
  let decl = of_class_decl ~pathloc type_ in
  anchor ~pathloc id
  <:html<
  <div class="class">
    $keyword "class"$$virtual_$$params$ $str:name$ : $decl$
    $doc$
  </div>
  >>

let of_class_type ~pathloc { ClassType.id; doc; virtual_; params; expr } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let params = of_class_params params in
  let expr = of_class_type_expr ~pathloc expr in
  anchor ~pathloc id
  <:html<
  <div class="classtype">
    $keyword "class type"$$virtual_$$params$ $str:name$ = $expr$
    $doc$
  </div>
  >>

let module_declaration
    ?(extra_classes=[]) ?(title_fn=(fun x -> x)) ~id ~pathloc
    name rhs doc rest =
  let classes = String.concat " " ("module"::extra_classes) in
  let title = title_fn <:html<$keyword "module"$ $name$>> in
  let id = Identifier.any id in
  let anchor html = if List.mem "ocamlary-doc" extra_classes
    then html
    else anchor ~pathloc id html
  in
  anchor
  <:html<
  <div class=$str:classes$>
      <div class="intro">$title$ $rhs$</div>
      $doc$
      $rest$
  </div>
  >>

let module_type_declaration ~id ~pathloc name rhs doc rest =
  anchor ~pathloc id
  <:html<
  <div class="modtype">
    <div class="intro">
      $keyword "module type"$ $str:name$ $rhs$
    </div>
    $doc$
    $rest$
  </div>
  >>

(* Predicate for module type expressions that don't have a signature body
   in the primary expression. *)
let rec is_short_sig = ModuleType.(function
  | Path _ -> true
  | Signature _ -> false
  | Functor (_, expr) -> is_short_sig expr
  | With (expr, _) -> is_short_sig expr
  | TypeOf (Module.Alias _) -> true
  | TypeOf (Module.ModuleType expr) -> is_short_sig expr
)

let rec base_of_module_type_expr ~pathloc = ModuleType.(function
  | TypeOf (Module.Alias (Path.Resolved path)) ->
    Some (Identifier.signature_of_module (Path.Resolved.identifier path))
  | Path (Path.Resolved path) ->
    Some (Identifier.signature_of_module_type (Path.Resolved.identifier path))
  | TypeOf (Module.Alias _)
  | Path _ -> None
  | Signature _ | Functor _ -> (* TODO: make sure anonymous paths are named *)
    Some pathloc.path
  | TypeOf (Module.ModuleType expr)
  | With (expr, _) -> base_of_module_type_expr ~pathloc expr
)

let rec of_module ~pathloc { Module.id; doc; type_ } =
  let id = Identifier.any id in
  let name = Identifier.name id in
  let rhs, rest = rhs_rest_of_decl ~pathloc type_ in
  let doc = maybe_div_doc ~pathloc doc in
  module_declaration ~id ~pathloc <:html<$str:name$>> rhs doc rest

and rhs_rest_of_decl ~pathloc = Module.(function
  | Alias path ->
    <:html<= $link_path ~pathloc (Path.any path)$>>, <:html<&>>
  | ModuleType module_type ->
    let rhs, rest = rhs_rest_of_sig ~pathloc module_type in
    <:html<: $rhs$>>, rest
)

and rhs_rest_of_sig ~pathloc = ModuleType.(function
  | Path path -> link_path ~pathloc (Path.any path), <:html<&>>
  | Signature s -> keyword "sig",
    let signature = of_signature ~pathloc s in
    <:html<
    <div class="sig">$signature$</div>
    <div class="outro">$keyword "end"$</div>
    >>
  | Functor (None, expr) -> (* TODO: test *)
    let rhs, rest = rhs_rest_of_sig ~pathloc expr in
    <:html<$keyword "functor"$ () &rarr; $rhs$>>, rest
  | Functor (Some (arg_ident, arg_sig), expr) ->
    let rhs_sig, rest_sig = rhs_rest_of_sig ~pathloc arg_sig in
    let rhs, rest = rhs_rest_of_sig ~pathloc expr in
    let arg = Identifier.name (Identifier.any arg_ident) in (* TODO: more? *)
    <:html<$keyword "functor"$ ($str:arg$ : $rhs_sig$$rest_sig$) &rarr; $rhs$>>,
    rest
  | With (With (expr, subs), subs') ->
    rhs_rest_of_sig ~pathloc (With (expr, subs @ subs'))
  | With (expr, subs) ->
    let base = base_of_module_type_expr ~pathloc expr in
    let rhs, rest = rhs_rest_of_sig ~pathloc expr in
    if is_short_sig expr
    then
      <:html<$rhs$ $keyword "with"$ $of_substitutions ~pathloc base [] subs$>>,
      rest
    else
      rhs,
      <:html<$rest$ $keyword "with"$ $of_substitutions ~pathloc base [] subs$>>
  | TypeOf (Module.Alias path) ->
    let path = Path.any path in
    <:html<$keyword "module type of"$ $link_path ~pathloc path$>>, <:html<&>>
  | TypeOf (Module.ModuleType module_type) ->
    let rhs, rest = rhs_rest_of_sig ~pathloc module_type in
    <:html<$keyword "module type of"$ $rhs$>>, rest
)

and of_substitutions ~pathloc base acc = ModuleType.(function
  | (ModuleEq (module_frag, module_eqn))::subs ->
    let rhs, rest = rhs_rest_of_decl ~pathloc module_eqn in
    let name = link_fragment ~pathloc base (Fragment.any module_frag) in
    of_substitutions ~pathloc base
      (<:html<$keyword "module"$ $name$ $rhs$ $rest$>> :: acc) subs
  | (TypeEq (type_frag, type_eqn))::subs ->
    let { TypeDecl.Equation.params; private_; manifest } = type_eqn in
    let name = link_fragment ~pathloc base (Fragment.any type_frag) in
    let params = of_type_params params in
    let manifest = match manifest with
      | Some m ->
        let p=if private_ then <:html<$keyword "private"$ >> else <:html<&>> in
        <:html< = $p$$of_type_expr ~pathloc m$>>
      | None -> <:html<&>>
    in
    of_substitutions ~pathloc base
      (<:html<$keyword "type"$ $params$$name$$manifest$>> :: acc) subs
  | (ModuleSubst (module_frag, module_path))::subs ->
    let rhs = link_path ~pathloc (Path.any module_path) in
    let name = link_fragment ~pathloc base (Fragment.any module_frag) in
    of_substitutions ~pathloc base
      (<:html<$keyword "module"$ $name$ := $rhs$>> :: acc) subs
  | (TypeSubst (type_frag, params, type_path))::subs ->
    let name = link_fragment ~pathloc base (Fragment.any type_frag) in
    let params =
      of_type_params (List.map (fun p -> (TypeDecl.Var p, None)) params)
    in
    let type_path = link_path ~pathloc (Path.any type_path) in
    of_substitutions ~pathloc base
      (<:html<$keyword "type"$ $params$$name$ := $params$$type_path$>> :: acc)
      subs
  | [] -> match List.rev acc with
    | [] -> <:html<&>>
    | [one] -> one
    | h::t -> fold_html <:html< $keyword "and"$ >> h t
)

and of_module_type ~pathloc { ModuleType.id; doc; expr } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let rhs, rest = match expr with
    | None -> <:html<&>>, <:html<&>>
    | Some expr ->
      let rhs, rest = rhs_rest_of_sig ~pathloc expr in
      <:html<= $rhs$>>, rest
  in
  module_type_declaration ~id ~pathloc name rhs doc rest

and of_include ~pathloc module_type_expr =
  let rhs, rest = rhs_rest_of_sig ~pathloc module_type_expr in
  <:html<
    <div class="include">
    $keyword "include"$ $rhs$ $rest$
    </div>
  >> (* TODO: test *)

and of_signature_item ~pathloc = Signature.(function
  | Value val_ -> Some (of_value ~pathloc val_)
  | External ext -> Some (of_external ~pathloc ext)
  | Type type_ -> Some (of_type ~pathloc type_)
  | TypExt ext -> Some (of_type_ext ~pathloc ext)
  | Exception exn -> Some (of_exception ~pathloc exn)
  | Class class_ -> Some (of_class ~pathloc class_)
  | ClassType class_type -> Some (of_class_type ~pathloc class_type)
  | Module module_ -> Some (of_module ~pathloc module_)
  | ModuleType module_type -> Some (of_module_type ~pathloc module_type)
  | Include module_type_expr -> Some (of_include ~pathloc module_type_expr)
  | Comment (Documentation.Documentation doc) ->
    Some (maybe_div_doc ~pathloc doc)
  | Comment Documentation.Stop -> None
)

and of_signature ~pathloc signature =
  let html_list = fold_doc_items (of_signature_item ~pathloc) [] signature in
  <:html<$list:html_list$>>

let of_top_module ~pathloc { Module.id; doc; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = link_ident ~pathloc () id in
  let rhs, rest = rhs_rest_of_decl ~pathloc type_ in
  let extra_classes = ["ocamlary-doc"] in
  let title_fn x = <:html<<h1 class="title">$x$</h1>&>> in
  module_declaration ~extra_classes ~title_fn ~id ~pathloc name rhs doc rest

let of_unit ~pathloc { Unit.id; doc; digest; imports; items } =
  (* TODO: more? *)
  of_top_module ~pathloc Module.({
    id; doc; type_ = ModuleType (ModuleType.Signature items);
  })
