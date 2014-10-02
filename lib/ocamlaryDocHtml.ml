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

module OpamModule = OpamDocPath.Module

type pathloc = {
  index_depth   : int;
  doc_base      : Uri.t;
  unit_depth    : int;
  internal_path : OpamModule.t;
}

let pathloc ~index_depth ~doc_base internal_path =
  let unit_depth = OpamModule.fold_left (fun i _ -> i + 1) 0 internal_path in
  {
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

let rec href_of_module ~pathloc modu =
  let name = OpamModule.(Name.to_string (name modu)) in
  match OpamModule.parent modu with
  | None ->
    let lib = OpamModule.library modu in
    Uri.(resolve "http"
           (href_of_library ~pathloc lib)
           (of_string (name ^ "/")))
  | Some p -> hctx (href_of_module ~pathloc p) ("module:"^name)

let rec id_of_path path name = match OpamModule.parent path with
  | None -> name
  | Some p ->
    id_of_path p ("module:"^OpamModule.(Name.to_string (name path))^"/"^name)

let rec link_module_path ?text ~pathloc modu =
  let name = OpamModule.(Name.to_string (name modu)) in
  let href = href_of_module ~pathloc modu in
  match text with
  | None ->
    let path = match OpamModule.parent modu with
      | None -> <:html<&>>
      | Some p -> <:html<$link_module_path ~pathloc p$.>>
    in
    <:html<$path$<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>

let maybe_link_module_path ?text ~pathloc = function
  | Known path -> link_module_path ?text ~pathloc path
  | Unknown s  -> <:html<$str:s$>>

let link_module_type_path ?text ~pathloc path =
  let open OpamDocPath.ModuleType in
  let name = Name.to_string (name path) in
  let modu = parent path in
  let phref = href_of_module ~pathloc modu in
  let href = hctx phref ("modtype:" ^ name) in
  match text with
  | None ->
    let phtml = link_module_path ~pathloc modu in
    <:html<$phtml$.<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>

let maybe_link_module_type_path ?text ~pathloc = function
  | Known path -> link_module_type_path ?text ~pathloc path
  | Unknown s  -> <:html<$str:s$>>

let link_type_path ?text ~pathloc path =
  let open OpamDocPath.Type in
  let name = Name.to_string (name path) in
  let modu = parent path in
  let phref = href_of_module ~pathloc modu in
  let href = hctx phref ("type:" ^ name) in
  match text with
  | None ->
    let phtml = link_module_path ~pathloc modu in
    <:html<$phtml$.<a href=$uri:href$>$str:name$</a>&>>
  | Some html ->
    <:html<<a href=$uri:href$>$html$</a>&>>

let maybe_link_type_path ?text ~pathloc = function
  | Known path -> link_type_path ?text ~pathloc path
  | Unknown s  -> <:html<$str:s$>>

let link_val_path ?text ~pathloc path =
  let open OpamDocPath.Value in
  let name = Name.to_string (name path) in
  let modu = parent path in
  let phref = href_of_module ~pathloc modu in
  let href = hctx phref ("val:" ^ name) in
  match text with
  | None ->
    let phtml = link_module_path ~pathloc modu in
    <:html<$phtml$.<a href=$uri:href$>$str:name$</a>&>>
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

let short_module_declaration name rhs doc =
  <:html<
    <div class="module">
      <div class="intro">
        $keyword "module"$ $str:name$ $rhs$
      </div>
      $doc$
    </div>
  >>

let short_module_type_declaration name rhs doc =
  <:html<
    <div class="module_type">
      <div class="intro">
        $keyword "module type"$ $str:name$ $rhs$
      </div>
      $doc$
    </div>
  >>

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
  | See (r, t) ->
    tag_fun "see-also" "See also" (of_text_elements t) (* TODO: use ref *)
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
  | Alias (t,v) -> (* TODO: test *)
    <:html<<span style="color:red">TODO alias $of_type_expr t$ $str:v$</span>&>>
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
  | Constr (path, [])  -> maybe_link_type_path ~pathloc path
  | Constr (path, [a]) ->
    <:html<$of_type_expr a$ $maybe_link_type_path ~pathloc path$>>
  | Constr (path, a::argl)  ->
    let a = of_type_expr a in
    let argl = List.map of_type_expr argl in
    let args = List.fold_left (fun phtml arghtml ->
      <:html<$phtml$, $arghtml$>>
    ) a argl in
    <:html<($args$) $maybe_link_type_path ~pathloc path$>>
  | TYPE_EXPR_todo s -> <:html<<span style="color:red">TODO: $str:s$</span>&>>

let of_val ~pathloc ({ name; doc; type_ }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name = OpamDocPath.Value.Name.to_string name in
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
  let name = OpamDocPath.Constructor.Name.to_string name in
  let sig_ = args_of_constructor ~pathloc args ret in
  let doc = maybe_span_doc ~pathloc doc in
  <:html<<div class="constr">| $str:name$$sig_$$doc$</div>&>>

let of_field ~pathloc ({ name; doc; type_ } : field) =
  let name = OpamDocPath.Field.Name.to_string name in
  let doc = maybe_span_doc ~pathloc doc in
  let thtml = of_type_expr ~pathloc type_ in
  <:html<<div class="field">$str:name$ : $thtml$;$doc$</div>&>>

let of_type_decl ~pathloc = function
  | Variant constrs ->
    <:html<$list:List.map (of_constructor ~pathloc) constrs$>>
  | Record fields -> <:html<{$list:List.map (of_field ~pathloc) fields$}>>
  | TYPE_todo s -> <:html<<span style="color:red">TODO: $str:s$</span>&>>

let of_type ~pathloc ({ name; doc; param; manifest; decl }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name = OpamDocPath.Type.Name.to_string name in
  let params = match param with
    | []  -> <:html<&>>
    | [p] -> <:html<'$str:p$ >>
    | ps  ->
      let type_vars = List.map (function "_" -> "_" | v -> "'"^v) ps in
      <:html<($str:String.concat ", " type_vars$) >>
  in
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

let of_exn ~pathloc ({ name; doc; args; ret }) =
  let doc = maybe_div_doc ~pathloc doc in
  let name = OpamDocPath.Exn.Name.to_string name in
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

let rec of_nested_module doc_state ~pathloc nested =
  let ({ name; doc; desc } : nested_module) = nested in
  let doc = maybe_div_doc ~pathloc doc in
  let path = OpamModule.create_submodule pathloc.internal_path name in
  let name = OpamModule.Name.to_string name in
  match desc with
  | Alias path ->
    short_module_declaration name
    <:html<= $maybe_link_module_path ~pathloc path$>> doc
  | Type (Path path) ->
    short_module_declaration name
    <:html<: $maybe_link_module_type_path ~pathloc path$>> doc
  | Type Signature ->
    let id = id_of_path pathloc.internal_path ("module:" ^ name) in
    let inner : module_ = OpamDocState.load_module doc_state path in
    let pathloc = { pathloc with
      unit_depth = pathloc.unit_depth + 1;
      internal_path = path;
    } in
    let signature = of_module_type_expr doc_state ~pathloc inner.type_ in
    <:html<
    <div id=$str:id$>
    <div class="module">
      <div class="intro">$keyword "module"$ $str:name$ : $keyword "sig"$</div>
      $doc$
      <div class="sig">
        $signature$
      </div>
      <div class="outro">$keyword "end"$</div>
    </div>
    </div>
    >>
  | MODULE_todo s -> <:html<<span style="color:red">TODO: $str:s$</span>&>>

and of_nested_module_type doc_state ~pathloc (nested : nested_module_type) =
  let { name; doc; desc } = nested in
  let doc = maybe_div_doc ~pathloc doc in
  let path = OpamDocPath.ModuleType.create pathloc.internal_path name in
  let name = OpamDocPath.ModuleType.Name.to_string name in
  match desc with
  | Manifest Signature ->
    let id = id_of_path pathloc.internal_path ("modtype:" ^ name) in
    let inner = OpamDocState.load_module_type doc_state path in
    let pathloc = { pathloc with
      unit_depth = pathloc.unit_depth + 1;
      (*internal_path = path;*)
      (* opam-doc-base is broken here. Nested signatures/modules won't work. *)
    } in
    let signature = of_module_type_expr doc_state ~pathloc inner.expr in
    <:html<
    <div id=$str:id$>
    <div class="module_type">
      <div class="intro">
        $keyword "module type"$ $str:name$ = $keyword "sig"$
      </div>
      $doc$
      <div class="sig">
        $signature$
      </div>
      <div class="outro">$keyword "end"$</div>
    </div>
    </div>
    >>
  | Manifest (Path path) ->
    short_module_type_declaration name
    <:html<= $maybe_link_module_type_path ~pathloc path$>> doc
  | Abstract -> short_module_type_declaration name <:html<&>> doc
  | MODULE_TYPE_todo s -> <:html<<span style="color:red">TODO: $str:s$</span>&>>

and of_signature_item doc_state ~pathloc : signature_item -> Cow.Html.t =
  function
  | Val val_ -> of_val ~pathloc val_
  | Types types -> <:html<$list:List.map (of_type ~pathloc) types$>>
  | Modules modules ->
    <:html<$list:List.map (of_nested_module doc_state ~pathloc) modules$>>
  | ModuleType module_type ->
    of_nested_module_type doc_state ~pathloc module_type
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

let of_module doc_state ~pathloc (modu : module_) =
  let path = link_module_path ~pathloc modu.path in
  let title = <:html<<h1 class="title">Module $path$</h1>&>> in
  let alias = match modu.alias with
    | None -> <:html<&>>
    | Some (Unknown s) -> <:html<= $str:s$>>
    | Some (Known path) -> <:html<= $link_module_path ~pathloc path$>>
  in
  let module_type_path = match modu.type_path with
    | None -> <:html<&>>
    | Some (Unknown s) -> <:html<: $str:s$>>
    | Some (Known path) -> <:html<: $link_module_type_path ~pathloc path$>>
  in
  let doc = maybe_div_doc ~pathloc modu.doc in
  <:html<
  <div class="ocamlary-doc module">
    $title$
    $alias$
    $module_type_path$
    $doc$
    <div class="sig">
      $of_module_type_expr doc_state ~pathloc modu.type_$
    </div>
  </div>
  >>

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
  let modules = List.rev_map OpamModule.Name.to_string library.modules in
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
