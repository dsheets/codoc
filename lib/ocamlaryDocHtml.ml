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

open OpamDocTypes

type pathloc = {
  index_depth   : int;
  doc_base      : Uri.t;
  unit_depth    : int;
  internal_path : OpamDocPath.parent;
}

let pathloc ~index_depth ~doc_base internal_path =
  let unit_depth = List.fold_left (fun i _ -> i + 1) 0
    OpamDocPath.(parent_list internal_path)
  in {
    index_depth; doc_base; unit_depth = unit_depth - 1; internal_path;
  }

let keyword text = <:html<<span class="keyword">$str:text$</span>&>>

let rec ascent_of_depth tl = function
  | 0 -> tl
  | n -> ascent_of_depth ("../" ^ tl) (n - 1)

let href_of_project ~pathloc project =
  Uri.of_string ((ascent_of_depth "" pathloc.index_depth) ^ project ^ "/")

let href_of_package ~pathloc pkg =
  let pkg_s = (OpamPackage.to_string pkg) ^ "/" in
  let project = OpamPackage.(Name.to_string (name pkg)) in
  Uri.(resolve "http" (href_of_project ~pathloc project) (of_string pkg_s))

let href_of_library ~pathloc lib =
  let pkg = OpamLibrary.package lib in
  let lib_name = OpamLibrary.(Name.to_string (name lib)) in
  Uri.(resolve "http"
         (resolve "http" (href_of_package ~pathloc pkg) pathloc.doc_base)
         (of_string (lib_name ^ "/")))

let hctx ctx (name : string) = match Uri.fragment ctx with
  | None -> Uri.with_fragment ctx (Some name)
  | Some frag -> Uri.with_fragment ctx (Some (frag^"/"^name))

let rec href_of_parent ~pathloc = function
  | OpamDocPath.Lib lib -> href_of_library ~pathloc lib
  | OpamDocPath.Module modu ->
    let name = OpamDocName.Module.to_string (OpamDocPath.Module.name modu) in
    let p = OpamDocPath.Module.parent modu in
    hctx (href_of_parent ~pathloc p) ("module:"^name)
  | OpamDocPath.ModType modtype ->
    let name = OpamDocPath.ModuleType.name modtype in
    let name = OpamDocName.ModuleType.to_string name in
    let p = OpamDocPath.ModuleType.parent modtype in
    hctx (href_of_parent ~pathloc p) ("modtype:"^name)

let rec id_of_path path name = OpamDocPath.(match path with
  | Lib _ -> name
  | Module modu -> id_of_path
    (Module.parent modu)
    ("module:"^OpamDocName.Module.to_string (Module.name modu)^"/"^name)
  | ModType modtype -> id_of_path
    (ModuleType.parent modtype)
    ("modtype:"^OpamDocName.ModuleType.to_string (ModuleType.name modtype)
     ^"/"^name)
)

let id_of_path path name = OpamDocPath.(match path with
  | Lib _ -> name
  | Module modu -> id_of_path (Module.parent modu) name
  | ModType modtype -> id_of_path (ModuleType.parent modtype) name
)

let rec link_module_path ?text ~pathloc modu = OpamDocPath.(
  let name = OpamDocName.Module.to_string (Module.name modu) in
  let href = href_of_parent ~pathloc (Module modu) in
  match text with
  | None ->
    let phtml = of_parent ~pathloc (Module.parent modu) in
    <:html<$phtml$<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>
)
and link_module_type_path ?text ~pathloc path = OpamDocPath.(
  let name = OpamDocName.ModuleType.to_string (ModuleType.name path) in
  let href = href_of_parent ~pathloc (ModType path) in
  match text with
  | None ->
    let phtml = of_parent ~pathloc (ModuleType.parent path) in
    <:html<$phtml$<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>
)
and of_parent ~pathloc = OpamDocPath.(function
  | Lib _ -> <:html<&>>
  | Module modu -> <:html<$link_module_path ~pathloc modu$.>>
  | ModType modtype -> <:html<$link_module_type_path ~pathloc modtype$.>>
)

let maybe_link_module_path ?text ~pathloc = function
  | Known path -> link_module_path ?text ~pathloc path
  | Unknown s  -> <:html<$str:s$>>

let maybe_link_module_type_path ?text ~pathloc = function
  | Known path -> link_module_type_path ?text ~pathloc path
  | Unknown s  -> <:html<$str:s$>>

let link_type_path ?text ~pathloc path =
  let name = OpamDocName.Type.to_string (OpamDocPath.Type.name path) in
  let parent = OpamDocPath.Type.parent path in
  let phref = href_of_parent ~pathloc parent in
  let href = hctx phref ("type:" ^ name) in
  match text with
  | None ->
    let phtml = of_parent ~pathloc parent in
    <:html<$phtml$<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>

let maybe_link_type_path ?text ~pathloc = function
  | Known path -> link_type_path ?text ~pathloc path
  | Unknown s  -> <:html<$str:s$>>

let link_val_path ?text ~pathloc path =
  let name = OpamDocName.Value.to_string (OpamDocPath.Value.name path) in
  let parent = OpamDocPath.Value.parent path in
  let phref = href_of_parent ~pathloc parent in
  let href = hctx phref ("val:" ^ name) in
  match text with
  | None ->
    let phtml = of_parent ~pathloc parent in
    <:html<$phtml$<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>

let section_attrs ?level label_opt =
  (* TODO: ids for unnamed sections? ocamldoc does it... *)
  let level_attrs = match level with
    | None       -> ["class","section"]
    | Some level -> ["class","section level_"^(string_of_int level)]
  in
  let label_attrs = match label_opt with
    | None -> []
    | Some label -> ["id","section:"^label]
  in
  level_attrs@label_attrs

let rec of_text_element ~pathloc txt =
  let of_text_elements = of_text_elements ~pathloc in
  match txt with
  | Raw s -> <:html<$str:s$>>
  | Code s -> <:html<<code>$str:s$</code>&>>
  | PreCode s -> <:html<<pre><code>$str:s$</code></pre>&>>
  | Verbatim s -> <:html<<pre>$str:s$</pre>&>>
  | Ref (Link href, None) ->
    <:html<<a href=$str:href$>REF_LINK NONE: $str:href$</a>&>> (* TODO: test *)
  | Target (None,href) ->
    <:html<<a href=$str:href$>TARGET NONE: $str:href$</a>&>> (* TODO: test *)
  | Target (Some a,href) ->
    <:html<<a href=$str:href$>TARGET SOME: $str:a$</a>&>> (* TODO: test *)
  | Ref (Link href, Some t) -> (* hyperlink *)
    <:html<<a href=$str:href$>$of_text_elements t$</a>&>>
  | Style (Bold, els) ->
    <:html<<b>$of_text_elements els$</b>&>>
  | Style (Italic, els) ->
    <:html<<i>$of_text_elements els$</i>&>>
  | Style (Emphasize, els) ->
    <:html<<em>$of_text_elements els$</em>&>>
  | Style (Center, els) ->
    <:html<<div class="doc-center">$of_text_elements els$</div>&>>
  | Style (Left, els) ->
    <:html<<div class="doc-left">$of_text_elements els$</div>&>>
  | Style (Right, els) ->
    <:html<<div class="doc-right">$of_text_elements els$</div>&>>
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
    <:html<<br />&>> (* TODO: use <p> *)
  | Title (1,label_opt,els) ->
    <:html<<h1 $alist:section_attrs label_opt$>$of_text_elements els$</h1>&>>
  | Title (2,label_opt,els) ->
    <:html<<h2 $alist:section_attrs label_opt$>$of_text_elements els$</h2>&>>
  | Title (3,label_opt,els) ->
    <:html<<h3 $alist:section_attrs label_opt$>$of_text_elements els$</h3>&>>
  | Title (4,label_opt,els) ->
    <:html<<h4 $alist:section_attrs label_opt$>$of_text_elements els$</h4>&>>
  | Title (5,label_opt,els) ->
    <:html<<h5 $alist:section_attrs label_opt$>$of_text_elements els$</h5>&>>
  | Title (6,label_opt,els) ->
    <:html<<h6 $alist:section_attrs label_opt$>$of_text_elements els$</h6>&>>
  | Title (level,label_opt,els) when level < 1 ->
    <:html<
    <h1 $alist:section_attrs ~level label_opt$>$of_text_elements els$</h1>
    >>
  | Title (level,label_opt,els) ->
    <:html<
    <h6 $alist:section_attrs ~level label_opt$>$of_text_elements els$</h6>
    >>
  | Ref (Module m, None) -> link_module_path ~pathloc m
  | Ref (Module m, Some els) -> (* TODO: test *)
    link_module_path ~text:<:html<$of_text_elements els$>> ~pathloc m
  | Ref (ModuleType m, None) -> link_module_type_path ~pathloc m
  | Ref (ModuleType m, Some els) -> (* TODO: test *)
    link_module_type_path ~text:<:html<$of_text_elements els$>> ~pathloc m
  | Ref (Type t, None) -> link_type_path ~pathloc t
  | Ref (Type t, Some els) -> (* TODO: test *)
    link_type_path ~text:<:html<$of_text_elements els$>> ~pathloc t
  | Ref (Val v, None) -> link_val_path ~pathloc v
  | Ref (Val v, Some els) -> (* TODO: test *)
    link_val_path ~text:<:html<$of_text_elements els$>> ~pathloc v
  | TEXT_todo s -> <:html<<span style="color: red">TODO: $str:s$</span>&>>
and lis_of_elss ~pathloc elss = List.map (fun els ->
  let f = of_text_element ~pathloc in
  <:html<<li>$list:List.map f els$</li>&>>
) elss
and of_text_elements ~pathloc els =
  let f = of_text_element ~pathloc in
  <:html<$list:List.map f els$>>

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
  match tag with
  | Author s ->
    tag_fun "author" "Author" <:html<$str:s$>>
  | Version s ->
    tag_fun "version" "Version" <:html<$str:s$>>
  | See (Documentation.See_url url_s, t) ->
    tag_fun "see-also" "See also"
      (of_text_elements ((Ref (Link url_s, Some [Raw url_s])) :: Raw " " :: t))
  | See (r, t) ->
    (* TODO: use file and doc ref *)
    tag_fun "see-also" "See also" (of_text_elements t)
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

let maybe_div_doc ~pathloc ({ info; tags }) =
  match info, tags with
  | [], [] -> <:html<&>>
  | text, tags ->
    <:html<
      <div class="doc">
        $of_text_elements ~pathloc text$
        $list:List.map (map_tag ~pathloc div_tag) tags$
      </div>
    >>

let maybe_span_doc ~pathloc doc = match doc.info with
  | [] -> <:html<&>>
  | text ->
    let tags = List.map (map_tag ~pathloc span_tag) doc.tags in
    let comment = of_text_elements ~pathloc text in
    <:html< <span class="doc">(* $comment$$list:tags$ *)</span>&>>

let html_product = List.fold_left (fun phtml ehtml ->
  <:html<$phtml$ * $ehtml$>>
)

let rec of_type_expr ~pathloc expr =
  let of_type_expr = of_type_expr ~pathloc in
  match expr with
  | Var v when v = "_" -> <:html<_>>
  | Var v -> <:html<'$str:v$>>
  | Alias (t,v) -> (* TODO: parens are only sometimes required *)
    <:html<($of_type_expr t$ $keyword "as"$ '$str:v$)&>>
  | Arrow (None, t, t') -> <:html<$of_type_expr t$ -> $of_type_expr t'$>>
  | Arrow (Some (Label l), t, t') ->
    <:html<$str:l$:$of_type_expr t$ -> $of_type_expr t'$>>
  | Arrow (Some (Default l), t, t') ->
    <:html<?$str:l$:$of_type_expr t$ -> $of_type_expr t'$>>
  | Tuple []       -> <:html<()>>
  | Tuple (e::els) ->
    let e = of_type_expr e in
    let els = List.map of_type_expr els in
    <:html<($html_product e els$)>>
  | Constr (path, argl) -> of_type_constr ~pathloc path argl
  | Variant { kind=Fixed; elements } ->
    <:html<[ $list:polyvar_elements ~pathloc elements$ ]&>>
  | Variant { kind=Closed csl; elements } -> (* TODO: test csl *)
    let sl = String.concat ", " csl in
    let els = polyvar_elements ~pathloc elements in
    <:html<<span style="color:red">TODO closed($str:sl$) [&lt; $list:els$ ]</span>&>>
  | Variant { kind=Open; elements } ->
    <:html<[&gt; $list:polyvar_elements ~pathloc elements$ ]&>>
  | Object { methods=[]; open_ } ->
    <:html<&lt; $str:if open_ then ".." else ""$ &gt;>>
  | Object { methods=meth::meths; open_ } ->
    let mhtml = List.fold_left (fun html meth ->
      <:html<$html$; $of_object_method ~pathloc meth$&>>
    ) <:html<&>> meths in
    let rest = if open_ then "; .." else "" in
    <:html<&lt; $of_object_method ~pathloc meth$$mhtml$$str:rest$ &gt;>>
  | Poly ([], expr) -> of_type_expr expr
  | Poly (vars, expr) ->
    let vars = String.concat " " (List.map (fun v -> "'"^v) vars) in
    <:html<$str:vars$ . $of_type_expr expr$&>>
  | Class (path, argl) -> of_type_constr ~cons:"#" ~pathloc path argl
  | TYPE_EXPR_todo s -> <:html<<span style="color:red">TODO: $str:s$</span>&>>
and of_object_method ~pathloc ({ name; type_ }) =
  <:html<$str:name$ : $of_type_expr ~pathloc type_$&>>
and of_type_constr ?(cons="") ~pathloc path = function
  | []  -> maybe_link_type_path ~pathloc path
  | [a] ->
    let arg = of_type_expr ~pathloc a in
    <:html<$arg$ $str:cons$$maybe_link_type_path ~pathloc path$>>
  | a::argl ->
    let a = of_type_expr ~pathloc a in
    let argl = List.map (of_type_expr ~pathloc) argl in
    let args = List.fold_left (fun phtml arghtml ->
      <:html<$phtml$, $arghtml$>>
    ) a argl in
    <:html<($args$) $maybe_link_type_path ~pathloc path$>>
and of_constr_arg ~pathloc = function
  | Some typ -> of_type_expr ~pathloc typ
  | None     -> <:html<&>>
and polyvar_element ~pathloc : variant_element -> Cow.Html.t = function
  | Type (path, argl) -> of_type_constr ~pathloc path argl
  | Constructor (name, []) -> <:html<`$str:name$&>>
  | Constructor (name, arg::args) ->
    let arghtml = List.fold_left (fun html arg ->
      <:html<$html$ &amp; $of_constr_arg ~pathloc arg$&>>
    ) (of_constr_arg ~pathloc arg) args in
    <:html<`$str:name$ of $arghtml$&>>
and polyvar_elements ~pathloc = List.map (fun pve ->
  <:html<| $polyvar_element ~pathloc pve$ &>>
)

let of_val ~pathloc ({ name; doc; type_ }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name = OpamDocName.Value.to_string name in
  let id = id_of_path pathloc.internal_path ("val:" ^ name) in
  <:html<
  <div id=$str:id$>
  <div class="val">
    $keyword "val"$ $str:name$ : $of_type_expr ~pathloc type_$
    $doc$
  </div>
  </div>
  >>

let args_of_constructor ~pathloc args ret =
  let of_type_expr = of_type_expr ~pathloc in
  match args, ret with
  | [],       None    -> <:html<&>>
  | a::args,  None    ->
    let a = of_type_expr a in
    let args = List.map of_type_expr args in
    let arghtml = html_product a args in
    <:html< $keyword "of"$ $arghtml$>>
  | [],       Some rt -> <:html< : $of_type_expr rt$>>
  | a::args,  Some rt ->
    let a = of_type_expr a in
    let args = List.map of_type_expr args in
    let arghtml = html_product a args in
    <:html< : $arghtml$ -> $of_type_expr rt$>>

let of_constructor ~pathloc ({ name; doc; args; ret } : constructor) =
  let name = OpamDocName.Constructor.to_string name in
  let sig_ = args_of_constructor ~pathloc args ret in
  let doc = maybe_span_doc ~pathloc doc in
  <:html<<div class="constr">| $str:name$$sig_$$doc$</div>&>>

let of_field ~pathloc ({ name; doc; type_ } : field) =
  let name = OpamDocName.Field.to_string name in
  let doc = maybe_span_doc ~pathloc doc in
  let thtml = of_type_expr ~pathloc type_ in
  <:html<<div class="field">$str:name$ : $thtml$;$doc$</div>&>>

let of_type_decl ~pathloc : type_decl -> Cow.Html.t = function
  | Variant constrs ->
    <:html<$list:List.map (of_constructor ~pathloc) constrs$>>
  | Record fields -> <:html<{$list:List.map (of_field ~pathloc) fields$}>>
  | Extensible -> <:html<..>>

let of_type_params = function
    | []  -> <:html<&>>
    | [p] -> <:html<'$str:p$ >>
    | ps  ->
      let type_vars = List.map (function "_" -> "_" | v -> "'"^v) ps in
      <:html<($str:String.concat ", " type_vars$) >>

let of_type ~pathloc ({ name; doc; param; manifest; decl }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name = OpamDocName.Type.to_string name in
  let params = of_type_params param in
  let manifest = match manifest with
    | None -> <:html<&>>
    | Some t -> <:html< = $of_type_expr ~pathloc t$>>
  in
  let decl = match decl with
    | None -> <:html<&>>
    | Some d -> <:html< = $of_type_decl ~pathloc d$>>
  in
  let id = id_of_path pathloc.internal_path ("type:" ^ name) in
  <:html<
  <div id=$str:id$>
  <div class="type">
    $keyword "type"$ $params$$str:name$$manifest$$decl$
    $doc$
  </div>
  </div>
  >>

let of_type_ext ~pathloc
    ({ type_path; doc; type_params; private_; constructors }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name_link = maybe_link_type_path ~pathloc type_path in
  let params = of_type_params type_params in
  let private_ = if private_ then keyword "private" else <:html<&>> in
  let constrs = List.map (of_constructor ~pathloc) constructors in
  (* TODO: define an ident scheme for typexts based on constrs *)
  let id = id_of_path pathloc.internal_path ("typext") in
  <:html<
  <div id=$str:id$>
  <div class="typext">
    $keyword "type"$ $params$$name_link$ += $private_$$list:constrs$
    $doc$
  </div>
  </div>
  >>

let of_exn ~pathloc ({ name; doc; args; ret }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name = OpamDocName.Constructor.to_string name in
  let args = args_of_constructor ~pathloc args ret in
  let id = id_of_path pathloc.internal_path ("exception:" ^ name) in
  <:html<
  <div id=$str:id$>
  <div class="exception">
    $keyword "exception"$ $str:name$$args$
    $doc$
  </div>
  </div>
  >>

let module_declaration
    ?(extra_classes=[]) ?(title_fn=(fun x -> x)) ?id ~pathloc
    name (modu : module_) sig_ =
  let classes = String.concat " " ("module"::extra_classes) in
  let title = title_fn <:html<$keyword "module"$ $name$>> in
  let alias = match modu.alias with
    | None -> <:html<&>>
    | Some path -> <:html<= $maybe_link_module_path ~pathloc path$>>
  in
  let sig_outro = <:html<
      <div class="sig">$sig_$</div>
      <div class="outro">$keyword "end"$</div>
      >> in
  let module_type_path, rest = match modu with
    | { type_path=None; type_=None } -> <:html<&>>, <:html<&>>
    | { type_path=Some path; type_=None } ->
      <:html<: $maybe_link_module_type_path ~pathloc path$>>, <:html<&>>
    | { type_path=None; type_=Some _ } ->
      <:html<: $keyword "sig"$>>, sig_outro
    | { type_path=Some path; type_=Some _ } ->
      <:html<: $maybe_link_module_type_path ~pathloc path$ = $keyword "sig"$>>,
      sig_outro
  in
  let doc = maybe_div_doc ~pathloc modu.doc in
  let mhtml = <:html<
    <div class=$str:classes$>
      <div class="intro">$title$ $alias$ $module_type_path$</div>
      $doc$
      $rest$
    </div>
  >> in
  match id with
  | None -> mhtml
  | Some id -> <:html<<div id=$str:id$>$mhtml$</div>&>>

let module_type_declaration ~id name rhs doc rest =
  <:html<
    <div id=$str:id$>
    <div class="module_type">
      <div class="intro">
        $keyword "module type"$ $str:name$ $rhs$
      </div>
      $doc$
      $rest$
    </div>
    </div>
  >>

let rec of_module doc_state ~pathloc (modu : module_) =
  let path = modu.path in
  let name = OpamDocName.Module.to_string (OpamDocPath.Module.name path) in
  let id = id_of_path pathloc.internal_path ("module:" ^ name) in
  let pathloc = { pathloc with
    unit_depth = pathloc.unit_depth + 1;
    internal_path = OpamDocPath.Module path;
  } in
  let sig_ = of_module_type_expr doc_state ~pathloc modu.type_ in
  module_declaration ~id ~pathloc <:html<$str:name$>> modu sig_

and of_module_type doc_state ~pathloc (modtype : module_type) =
  let path = modtype.path in
  let doc = maybe_div_doc ~pathloc modtype.doc in
  let name =
    OpamDocName.ModuleType.to_string (OpamDocPath.ModuleType.name path)
  in
  let id = id_of_path pathloc.internal_path ("modtype:" ^ name) in
  let sig_outro expr =
    let pathloc = { pathloc with
      unit_depth = pathloc.unit_depth + 1;
      internal_path = OpamDocPath.ModType path;
    } in
    <:html<
      <div class="sig">$of_module_type_expr doc_state ~pathloc expr$</div>
      <div class="outro">$keyword "end"$</div>
    >> in
  let rhs, rest = match modtype with
    | { alias=None; expr=None } -> <:html<&>>, <:html<&>>
    | { alias=None; expr } ->
      <:html<= $keyword "sig"$>>, sig_outro expr
    | { alias=Some path; expr=None } ->
      <:html<= $maybe_link_module_type_path ~pathloc path$>>, <:html<&>>
    | { alias=Some path; expr } ->
      <:html<= $maybe_link_module_type_path ~pathloc path$ = $keyword "sig"$>>,
      sig_outro expr
  in
  module_type_declaration ~id name rhs doc rest

and of_signature_item doc_state ~pathloc : signature_item -> Cow.Html.t =
  function
  | Val val_ -> of_val ~pathloc val_
  | Types types -> <:html<$list:List.map (of_type ~pathloc) types$>>
  | TypExt ext -> of_type_ext ~pathloc ext
  | Modules modules ->
    <:html<$list:List.map (of_module doc_state ~pathloc) modules$>>
  | ModuleType module_type ->
    of_module_type doc_state ~pathloc module_type
  | Comment { info } ->
    <:html<
    <div class="comment">$of_text_elements ~pathloc info$</div>
    >>
  | Exn exn -> of_exn ~pathloc exn
  | SIG_todo s -> <:html<<span style="color:red">TODO: $str:s$</span>&>>

and of_module_type_expr doc_state ~pathloc
    : module_type_expr option -> Cow.Html.t =
  function
  | None -> <:html<&>>
  | Some (Signature sig_items) ->
    <:html<$list:List.map (of_signature_item doc_state ~pathloc) sig_items$>>
  | Some (MODULE_TYPE_EXPR_todo descr) ->
    <:html<<span style="color:red">TODO: $str:descr$</span>&>>

let of_top_module doc_state ~pathloc (modu : module_) =
  let name = link_module_path ~pathloc modu.path in
  let extra_classes = ["ocamlary-doc"] in
  let title_fn x = <:html<<h1 class="title">$x$</h1>&>> in
  let sig_ = of_module_type_expr doc_state ~pathloc modu.type_ in
  module_declaration ~extra_classes ~title_fn ~pathloc name modu sig_

let libraries ({ Ocamlary.doc_state }) pkg =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  try
    let package = OpamDocState.load_package doc_state pkg in
    let libraries = List.rev_map (fun lib_name ->
      let library = OpamLibrary.create package.path lib_name in
      OpamDocState.load_library doc_state library
    ) package.libraries in
    Printf.eprintf "Loading libraries for %s succeeded (%d)\n%!" (OpamPackage.to_string pkg) (List.length libraries);
    libraries
  with e ->
    Printf.eprintf "Loading libraries for %s raised %s\n%!"
      (OpamPackage.to_string pkg)
      (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    []

let name_of_library (library : library) =
  OpamLibrary.(Name.to_string (name library.path))

let uri_of_library doc_base (library : library) =
  let dir = (name_of_library library) ^ "/" in
  Uri.(resolve "http" doc_base (of_string dir))

let link_of_library doc_base library =
  let name = name_of_library library in
  let uri = uri_of_library doc_base library in
  <:html<<a href=$uri:uri$>$str:name$</a>&>>

let uri_of_module_name doc_base library mod_name =
  let lib_uri = uri_of_library doc_base library in
  Uri.(resolve "http" lib_uri (of_string (mod_name ^ "/")))

let link_of_module_name doc_base library mod_name =
  let uri = uri_of_module_name doc_base library mod_name in
  <:html<<a href=$uri:uri$>$str: mod_name$</a>&>>

let index_of_library ~doc_base (library : library) =
  let modules = List.rev_map OpamDocName.Module.to_string library.modules in
  let sorted = List.sort String.compare modules in
  let html = List.map (fun mod_name ->
    <:html<<li>$link_of_module_name doc_base library mod_name$</li>&>>
  ) sorted in
  <:html<<div class="library">
    <h2>$link_of_library doc_base library$ (findlib)</h2>
    <ul>
      $list:html$
    </ul>
</div>&>>

let widget_of_libraries ~doc_base libraries =
  let sorted = List.sort (fun a b ->
    String.compare (name_of_library a) (name_of_library b)
  ) libraries in
  <:html<<div class="ocamlary">
    $list:(List.map (index_of_library ~doc_base) sorted)$
</div>&>>
