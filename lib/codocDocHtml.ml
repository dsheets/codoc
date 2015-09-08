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

module BlueTree = Blueprint.Tree

open DocOckTypes
open DocOckPaths

open CodocDocMaps
open CodocDoc

type 'a href = 'a * Uri.t

let link ?href text = BlueTree.(of_kv_maybe [
  "anchor", Some (of_list text);
  "href", begin match href with
    | None -> None
    | Some href -> Some (of_string (Uri.to_string href))
  end;
])
let txt text = [BlueTree.(of_cons "raw" (of_string text))]

class link_ident_map ?text ~pathloc () :
  [unit, root, Blueprint.t list] Identifier.any_fold =
object (self)
  inherit [unit, root, Blueprint.t list] Identifier.any_parent_fold

  method root () r name =
    let href = CodocUnit.Href.of_ident pathloc (Identifier.Root (r, name)) in
    [ link ?href (txt name) ]
  method core_type () name = [ link (txt name) ] (* TODO: link *)
  method core_exception () name = [ link (txt name) ] (* TODO: link *)
  method argument () parent i name =
    self#parent ()
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      (CodocUnit.Id.name_of_argument i name)
  method private parent () ident parent name =
    match CodocUnit.Href.of_ident pathloc ident, text with
    | None, None ->
      let phtml = Identifier.fold_any self () (Identifier.any parent) in
      (link (txt name))::phtml
    | Some href, None ->
      let phtml = Identifier.fold_any self () (Identifier.any parent) in
      (link ~href (txt name))::phtml
    | None, Some html -> [ link html ]
    | Some href, Some html -> [ link ~href html ]
end

let link_ident_map ?text ~pathloc () =
  new link_ident_map ?text ~pathloc ()

let link_ident ?text ~pathloc () =
  Identifier.fold_any (link_ident_map ?text ~pathloc ()) ()

class link_resolved_path_map ~pathloc link_path
  : [unit, root, Blueprint.t list, Path.kind] Path_resolved.any_fold =
object (self)
  inherit [unit, root, Blueprint.t list] Path_resolved.any_parent_fold

  method identifier () ident =
    let ident = Identifier.any ident in
    let text = txt (Identifier.name ident) in
    link_ident ~text ~pathloc () ident
  method subst () _rsub li =
    Path_resolved.(fold_any self () (any li))
  method subst_alias () _rsub li =
    Path_resolved.(fold_any self () (any li))
  method apply () func module_path =
    let html = Path_resolved.(fold_any self () (any func)) in
    BlueTree.([of_kv [
      "apply", of_kv [
        "fun", of_list html;
        "arg", of_list (link_path ~pathloc (Path.any module_path));
      ];
    ]])
  method private parent () path parent _name =
    let phtml = Path_resolved.(fold_any self () (any parent)) in
    let link = self#identifier () (Path.Resolved.identifier path) in
    link @ phtml
end

let link_resolved_path_map ~pathloc link_path =
  new link_resolved_path_map ~pathloc link_path

let link_resolved_path ~pathloc link_path =
  Path_resolved.fold_any (link_resolved_path_map ~pathloc link_path) ()

let rec link_path ~pathloc : ('a,'b) Path.t -> Blueprint.t list =
  Path.(function
  | Root root -> [ link (txt root) ]
  | Dot (parent, name) ->
    (link (txt name))::link_path ~pathloc (any parent)
  | Resolved resolved -> link_resolved_path ~pathloc link_path resolved
  | Apply (m, m') -> (* TODO: test *)
    BlueTree.([of_cons "apply" (of_kv [
      "fun", of_list (link_path ~pathloc (any m));
      "arg", of_list (link_path ~pathloc (any m'));
    ])])
  )

class link_resolved_fragment_map ~pathloc base
  : [unit, root, Blueprint.t list, Fragment.Resolved.kind, [`Branch]]
  Fragment_resolved.any_fold =
object (self)
  inherit [unit, root, Blueprint.t list, Fragment.Resolved.kind, [`Branch]]
    Fragment_resolved.any_parent_fold

  method subst () _rsub li = Fragment_resolved.fold_any self () li
  method subst_alias () _rsub li = Fragment_resolved.fold_any self () li

  method private parent () fragment parent name = Fragment_resolved.(
    let text = txt name in
    let link = match base with
      | Some base ->
        let ident = Identifier.any (identifier base fragment) in
        link_ident ~text ~pathloc () ident
      | None -> text
    in
    let rec render : root signature -> Blueprint.t list = function
      | Root -> link
      | Subst (_, li) -> render li
      | SubstAlias (_, li) -> render li
      | Module _ as parent -> (* need to capture GADT eq *)
        let phtml = fold_any self () parent in
        link @ phtml
    in render parent
  )
end

let link_resolved_fragment_map ~pathloc base =
  new link_resolved_fragment_map ~pathloc base

let link_resolved_fragment ~pathloc base =
  Fragment_resolved.fold_any (link_resolved_fragment_map ~pathloc base) ()

let rec link_fragment ~pathloc base : 'a Fragment.any -> Blueprint.t list =
  Fragment.(function
    | Resolved resolved -> link_resolved_fragment ~pathloc base resolved
    | Dot (Resolved Resolved.Root, name) -> [ link (txt name) ] (* TODO: test *)
    | Dot ((Dot _ | Resolved (Resolved.Module _)) as parent, name) ->
      (link (txt name))::link_fragment ~pathloc base (any parent)
    | signature -> match split signature with (* TODO: test *)
      | name, None -> [ link (txt name) ]
      | name, Some parent ->
        (link (txt name))::link_fragment ~pathloc base parent
  )

class link_resolved_reference_map ?text ~pathloc ()
  : [unit, root, Blueprint.t list, Reference.kind] Reference_resolved.any_fold =
object (self)
  inherit [unit, root, Blueprint.t list] Reference_resolved.any_parent_fold

  method identifier () ident =
    let ident = Identifier.any ident in
    let text = match text with
      | None -> txt (Identifier.name ident)
      | Some text -> text
    in
    link_ident ~text ~pathloc () ident
  method private parent () reference parent _name =
    let link = self#identifier () (Reference.Resolved.identifier reference) in
    match text with
    | None ->
      let phtml = Reference_resolved.(fold_any self () (any parent)) in
      link @ phtml
    | Some _ -> link
end

let link_resolved_reference_map ?text ~pathloc () =
  new link_resolved_reference_map ?text ~pathloc ()

let link_resolved_reference ?text ~pathloc () =
  Reference_resolved.fold_any (link_resolved_reference_map ?text ~pathloc ()) ()

(* TODO: output special unlinked ref *)
let rec link_reference ?text ~pathloc : ('a,'b) Reference.t -> Blueprint.t list =
  Reference.(function
  | Root root -> [ link (txt root) ]
  | Dot (parent, name) ->
    begin match text with
      | None -> (link (txt name))::link_reference ~pathloc (any parent)
      | Some html -> [link html]
    end
  | Resolved resolved -> link_resolved_reference ?text ~pathloc () resolved
  )

let tag_identifier = Identifier.(function
  | Root _
  | Module _
  | Argument _ -> "mod"
  | ModuleType _ -> "modtype"
  | Type _ | CoreType _ -> "type"
  | Constructor _ -> "cons"
  | Field _ -> "field"
  | Extension _ -> "ext"
  | Exception _ | CoreException _ -> "exn"
  | Value _ -> "val"
  | Class _ -> "class"
  | ClassType _ -> "classtype"
  | Method _ -> "method"
  | InstanceVariable _ -> "instance"
  | Label _ -> "section"
)

let tag_resolved_reference = Reference.Resolved.(function
  | Class _ -> "class"
  | ClassType _ -> "classtype"
  | Constructor _ -> "cons"
  | Extension _ -> "ext"
  | Exception _ -> "exn"
  | Field _ -> "field"
  | InstanceVariable _ -> "instance"
  | Label _ -> "section"
  | Method _ -> "method"
  | Module _ -> "mod"
  | ModuleType _ -> "modtype"
  | Type _ -> "type"
  | Value _ -> "val"
  | Identifier ident -> tag_identifier ident
)

let tag_reference = Reference.(function
  | Root _ -> BlueTree.tag "mod"
  | Dot _  -> BlueTree.tag "element"
  | Resolved resolved -> tag_resolved_reference resolved, BlueTree.empty ()
)

let region ~pathloc id html =
  let href = match CodocUnit.Href.id_of_ident pathloc id with
    | None -> None
    | Some id -> Some (BlueTree.of_string ("#"^id))
  in
  let id = match CodocUnit.Href.id_of_ident pathloc id with
    | None -> None
    | Some id -> Some (BlueTree.of_string id)
  in
  BlueTree.(of_kv_maybe ([
    "id", id;
    "body", Some html;
    "href", href;
  ]))

let label_region ~pathloc label_opt html = match label_opt with
  | None -> BlueTree.of_cons "body" html
  | Some label -> region ~pathloc (Identifier.any label) html

let rec of_text_element ~pathloc txt_ =
  let of_text_elements = of_text_elements ~pathloc in
  Reference.(Documentation.(BlueTree.(match txt_ with
  | Raw s -> of_cons_string "raw" s
  | Code "" -> of_cons_string "raw" "[]"
  | Code s -> of_cons_string "code" s
  | PreCode s -> of_cons_string "precode" s
  | Verbatim s -> of_cons_string "verbatim" s
  | Target (None,href) ->
    of_cons "target" (of_cons_string "href" href) (* TODO: test *)
  | Target (Some a,href) ->
    of_cons "target" (link ~href:(Uri.of_string href) (txt a)) (* TODO: test *)
  | Reference (Link href, None) ->
    of_cons "ref-link" (of_cons_string "href" href) (* TODO: test *)
  | Reference (Link href, Some t) -> (* hyperlink *)
    of_cons "ref-link" (link ~href:(Uri.of_string href) (of_text_elements t))
  | Style (Bold, els) ->
    of_cons "bold" (of_list (of_text_elements els))
  | Style (Italic, els) ->
    of_cons "italic" (of_list (of_text_elements els))
  | Style (Emphasize, els) ->
    of_cons "emph" (of_list (of_text_elements els))
  | Style (Center, els) ->
    of_cons "center" (paragraphs_of_text ~pathloc els)
  | Style (Left, els) ->
    of_cons "left" (paragraphs_of_text ~pathloc els)
  | Style (Right, els) ->
    of_cons "right" (paragraphs_of_text ~pathloc els)
  | Style (Superscript, els) ->
    of_cons "super" (of_list (of_text_elements els))
  | Style (Subscript, els) ->
    of_cons "sub" (of_list (of_text_elements els))
  | Style (Custom s, els) ->
    of_cons "custom" (of_kv [
      tag s;
      "type", of_string s;
      "text", of_list (of_text_elements els);
    ]) (* TODO: test *)
  | List elss ->
    of_cons "list" (of_list (lis_of_elss ~pathloc elss))
  | Enum elss ->
    of_cons "enum" (of_list (lis_of_elss ~pathloc elss))
  | Newline -> of_cons "nl" (empty ())
  | Title (level,label_opt,els) ->
    let conds = if level < 1 then [ "lt_1" ] else [] in
    let conds = if level > 6 then "gt_6"::conds else conds in
    let conds = if level > 9 then "gt_9"::conds else conds in
    let conds = if level > 99 then "gt_99"::conds else conds in
    let conds = List.rev_map tag conds in
    of_cons "title" (label_region ~pathloc label_opt (of_kv ([
      "level", of_string (string_of_int level);
      string_of_int level, empty ();
      "text", of_list (of_text_elements els);
    ]@conds)))
  | Reference (Module m, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any m));
            tag "mod" ]
  | Reference (Module m, Some els) ->
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any m));
            tag "mod" ]
  | Reference (ModuleType m, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any m));
            tag "modtype" ]
  | Reference (ModuleType m, Some els) ->
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any m));
            tag "modtype" ]
  | Reference (Type t, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any t));
            tag "type" ]
  | Reference (Type t, Some els) ->
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any t));
            tag "type" ]
  | Reference (Constructor c, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any c));
            tag "cons" ]
  | Reference (Constructor c, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any c));
            tag "cons" ]
  | Reference (Field f, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any f));
            tag "field" ]
  | Reference (Field f, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any f));
            tag "field" ]
  | Reference (Extension e, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any e));
            tag "ext" ]
  | Reference (Extension e, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any e));
            tag "ext" ]
  | Reference (Exception e, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any e));
            tag "exn" ]
  | Reference (Exception e, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any e));
            tag "exn" ]
  | Reference (Value v, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any v));
            tag "val" ]
  | Reference (Value v, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any v));
            tag "val" ]
  | Reference (Class c, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any c));
            tag "class" ]
  | Reference (Class c, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any c));
            tag "class" ]
  | Reference (ClassType c, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any c));
            tag "classtype" ]
  | Reference (ClassType c, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any c));
            tag "classtype" ]
  | Reference (Method m, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any m));
            tag "method" ]
  | Reference (Method m, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any m));
            tag "method" ]
  | Reference (InstanceVariable i, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any i));
            tag "instance" ]
  | Reference (InstanceVariable i, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any i));
            tag "instance" ]
  | Reference (Element e, None) -> (* syntactically unspecified *)
    let r = any e in
    of_kv [ "ref", of_list (link_reference ~pathloc r);
            tag_reference r ]
  | Reference (Element e, Some els) -> (* syntactically unspecified *)
    let text = of_text_elements els in
    let r = any e in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc r);
            tag_reference r ]
  | Reference (Section s, None) ->
    of_kv [ "ref", of_list (link_reference ~pathloc (any s));
            tag "section" ]
  | Reference (Section s, Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (link_reference ~text ~pathloc (any s));
            tag "section" ]
  | Reference (Custom (s,s'), None) -> (* TODO: test *)
    of_kv [ "ref", of_list [of_string s; of_string s'];
            tag "custom" ]
  | Reference (Custom (s,s'), Some els) -> (* TODO: test *)
    let text = of_text_elements els in
    of_kv [ "ref", of_list (of_string s::of_string s'::text);
            tag "custom" ]
  | Special (Modules mods) ->
    let mods_html = List.map (fun m -> of_kv [
      "ref", of_list (link_reference ~pathloc (any m));
      tag "mod";
    ]) mods in
    of_cons "modules" (of_list mods_html)
  | Special Index -> (* TODO: test *)
    of_cons "index" (empty ())
  )))
and lis_of_elss ~pathloc elss = List.map BlueTree.(fun els ->
  of_list (of_text_elements ~pathloc els)
) elss
and of_text_elements ~pathloc els =
  let f = of_text_element ~pathloc in
  List.map f els
and paragraphs_of_text ~pathloc els =
  let paras = List.map BlueTree.(function
    | Para els ->
      of_cons "para" (of_list (of_text_elements ~pathloc els))
    | Block els ->
      of_cons "block" (of_list (of_text_elements ~pathloc els))
  ) (paragraphize els) in
  BlueTree.of_list paras

let make_tag ?(rest=[]) label content =
  BlueTree.(of_kv ([
    label, of_list content;
    "type", of_string label;
  ]@rest))

let map_tag ~pathloc tag =
  let of_text_elements = of_text_elements ~pathloc in
  Documentation.(match tag with
  | Author s ->
    make_tag "author" (txt s)
  | Version s ->
    make_tag "version" (txt s)
  | See (Url url_s, t) ->
    let rest = [ "href", BlueTree.of_string url_s ] in
    make_tag ~rest "see-url" (of_text_elements t)
  | See (File file_s, t) -> (* TODO: test *)
    let rest = [ "file", BlueTree.of_string file_s ] in
    make_tag ~rest "see-file" (of_text_elements t)
  | See (Doc doc_s, t) -> (* TODO: test *)
    let rest = [ "doc", BlueTree.of_string doc_s ] in
    make_tag ~rest "see-doc" (of_text_elements t)
  | Since s ->
    make_tag "since" (txt s)
  | Before (s,t) ->
    let rest = [ "when", BlueTree.of_string s ] in
    make_tag ~rest "before" (of_text_elements t)
  | Deprecated t ->
    make_tag "deprecated" (of_text_elements t)
  | Param (s,t) ->
    let rest = [ "name", BlueTree.of_string s ] in
    make_tag ~rest "param" (of_text_elements t)
  | Raise (s,t) ->
    let rest = [ "name", BlueTree.of_string s ] in
    make_tag ~rest "raises" (of_text_elements t)
  | Return t ->
    make_tag "return" (of_text_elements t)
  | Tag (s, t) ->
    let rest = [ "tag", make_tag s [] ] in
    make_tag ~rest "custom" (of_text_elements t)
  )

let string_of_pos position = Documentation.Error.Position.(
  string_of_int position.line ^":"^ string_of_int position.column
)

let of_error = Documentation.Error.(BlueTree.(
  fun { offset; location; message } ->
    let location = Location.(match location with
      | None -> None
      | Some { filename; start; finish } ->
        let s = filename^":"^string_of_pos start^"-"^string_of_pos finish in
        Some (of_string s)
    ) in
    let offset = Offset.(
      of_string (string_of_pos offset.start ^"-"^ string_of_pos offset.finish)
    ) in
    of_kv_maybe [
      "location", location;
      "offset", Some offset;
      "message", Some (of_string message);
    ]
))

let maybe_doc ~pathloc = Documentation.(BlueTree.(function
  | Error error -> of_cons "error" (of_error error)
  | Ok { text; tags } -> match text, tags with
    | [], [] -> empty ()
    | text, tags ->
      of_kv [
        "descr", of_lazy_tree (fun () ->
          let text = first_sentence_of_text text in
          of_list (of_text_elements ~pathloc text)
        );
        "text", paragraphs_of_text ~pathloc text;
        "tags", of_list (List.map (map_tag ~pathloc) tags);
      ]
))

let parens = BlueTree.of_cons "parens"

let rec of_type_expr ?(group=false) ~pathloc expr =
  let of_type_expr = of_type_expr ~pathloc in
  TypeExpr.(BlueTree.(match expr with
  | Any | Var "_" -> of_kv [ tag "any" ]
  (* TODO: How are Any and Var _ different? *)
  | Var v -> of_kv_string [ "var", v ]
  | Alias (t,v) -> (* TODO: parens are only sometimes required *)
    of_cons "alias" (of_kv [
      "type", of_type_expr t;
      "var", of_string v;
    ])
  | Arrow (label, (Arrow _ as t), t') ->
    (* Tuple binds more tightly *)
    let v = of_cons "arrow" (of_kv [
      "domain", of_labeled_type_expr ~group:true ~pathloc t label;
      "range", of_type_expr t';
    ]) in
    if group then parens v else v
  | Arrow (label, t, t') ->
    let v = of_cons "arrow" (of_kv [
      "domain", of_labeled_type_expr ~pathloc t label;
      "range", of_type_expr t';
    ]) in
    if group then parens v else v
  | Tuple []       -> of_kv [ tag "unit" ]
  | Tuple els ->
    let els = List.map (of_type_expr ~group:true) els in
    let v = of_cons "tuple" (of_list els) in
    if group then parens v else v
  | Constr (path, argl) ->
    of_cons "cons" (of_type_constr ~pathloc path argl)
  | Variant { Variant.kind=Variant.Fixed; elements } ->
    of_cons "variant" (of_cons "fixed" (polyvar_elements ~pathloc elements))
  | Variant { Variant.kind=Variant.Closed csl; elements } -> (* TODO: test csl *)
    let els = polyvar_elements ~pathloc elements in
    begin match csl with
      | []  -> of_cons "variant" (of_cons "closed" els)
      | csl ->
        of_cons "variant" (of_kv [
          "closed", els;
          "open", of_list (List.map of_string csl);
        ])
    end
  | Variant { Variant.kind=Variant.Open; elements } ->
    of_cons "variant" (of_cons "open" (polyvar_elements ~pathloc elements))
  | Object { Object.methods; open_ } ->
    of_cons "object" (of_kv_maybe ([
      "methods", Some (of_list (List.map (of_object_method ~pathloc) methods));
      "open", if open_ then Some (empty ()) else None;
    ]))
  | Poly ([], expr) -> of_type_expr expr
  | Poly (vars, expr) ->
    of_cons "poly" (of_kv [
      "vars", of_list (List.map of_string vars);
      "expr", of_type_expr expr;
    ])
  | Class (path, argl) ->
    of_cons "class" (of_type_constr ~pathloc (Path.type_of_class_type path) argl)
  | Package { Package.path; substitutions; } ->
    of_cons "package" (of_kv [
      "path", of_list (link_path ~pathloc (Path.any path));
      "subs", of_list (List.map (of_package_sub ~pathloc path) substitutions);
    ])
  ))
and of_object_method ~pathloc { TypeExpr.Object.name; type_ } = BlueTree.(
  of_kv [ "name", of_string name; "type", of_type_expr ~pathloc type_ ]
)
and of_type_constr ~pathloc path = BlueTree.(function
  | []  -> of_cons "path" (of_list (link_path ~pathloc (Path.any path)))
  | [a] -> of_kv [
    "path", of_list (link_path ~pathloc (Path.any path));
    "args", of_list [of_type_expr ~group:true ~pathloc a];
  ]
  | args -> of_kv [
    "path", of_list (link_path ~pathloc (Path.any path));
    "args", of_list (List.map (of_type_expr ~pathloc) args);
  ]
)
and polyvar_element ~pathloc : 'a TypeExpr.Variant.element -> Blueprint.t =
  TypeExpr.Variant.(BlueTree.(function
    | Type expr -> of_cons "type" (of_type_expr ~pathloc expr)
    | Constructor (name, empty, args) -> of_cons "cons" (of_kv_maybe [
      "name", Some (of_string name);
      "args", Some (of_list (List.map (of_type_expr ~pathloc) args));
      "empty", if empty then Some (of_kv []) else None;
    ])
  ))
and polyvar_elements ~pathloc list =
  BlueTree.of_list (List.map (polyvar_element ~pathloc) list)
and of_labeled_type_expr ?(group=false) ~pathloc t = TypeExpr.(BlueTree.(function
  | None -> of_cons "type" (of_type_expr ~group ~pathloc t)
  | Some (Label l) -> of_kv [
    "type", of_type_expr ~group ~pathloc t;
    "label", of_string l;
  ]
  | Some (Optional l) -> of_kv [
    "type", of_type_expr ~group ~pathloc t;
    "optional", of_string l;
  ]
))
and of_package_sub ~pathloc path (fragment, expr) =
  let path = match path with
    | Path.Resolved resolved ->
      Some (Identifier.signature_of_module_type
              (Path.Resolved.identifier resolved))
    | _ -> None
  in
  let fragment_link = link_fragment ~pathloc path (Fragment.any fragment) in
  BlueTree.(of_kv [
    "fragment", of_list fragment_link;
    "type", of_type_expr ~pathloc expr;
  ])

let of_value ~pathloc { Value.id; doc; type_ } =
  let doc = maybe_doc ~pathloc doc in
  let ident = Identifier.any id in
  let name = Identifier.name ident in
  region ~pathloc ident BlueTree.(of_kv [
    "name", of_string name;
    "type", of_type_expr ~pathloc type_;
    "doc", doc;
  ])

let of_external ~pathloc { External.id; doc; type_; primitives } =
  let doc = maybe_doc ~pathloc doc in
  let ident = Identifier.any id in
  let name = Identifier.name ident in
  region ~pathloc ident BlueTree.(of_kv [
    "name", of_string name;
    "type", of_type_expr ~pathloc type_;
    "primitives", of_list (List.map of_string primitives);
    "doc", doc;
  ])

let args_of_constructor ~pathloc args res =
  let of_type_expr = of_type_expr ~pathloc in
  BlueTree.(match args, res with
  | [],       None    -> of_kv []
  | [TypeExpr.Arrow _ as a], None ->
    of_cons "args" (of_list [of_type_expr a])
  | args,  None    ->
    let args = List.map (of_type_expr ~group:true) args in
    of_cons "args" (of_list args)
  | [],       Some rt ->
    of_cons "type" (of_type_expr rt)
  | args,  Some rt ->
    let args = List.map (of_type_expr ~group:true) args in
    of_kv [
      "args", of_list args;
      "type", of_type_expr rt;
    ]
  )

let of_constructor ~pathloc { TypeDecl.Constructor.id; doc; args; res } =
  let id   = Identifier.any id in
  let name = Identifier.name id in
  let sig_ = args_of_constructor ~pathloc args res in
  let doc  = maybe_doc ~pathloc doc in
  region ~pathloc id BlueTree.(of_kv [
    "name", of_string name;
    "sig",  sig_;
    "doc",  doc;
  ])

let of_extension ~pathloc { Extension.Constructor.id; doc; args; res } =
  let id   = Identifier.any id in
  let name = Identifier.name (Identifier.any id) in
  let sig_ = args_of_constructor ~pathloc args res in
  let doc  = maybe_doc ~pathloc doc in
  region ~pathloc id BlueTree.(of_kv [
    "name", of_string name;
    "sig",  sig_;
    "doc",  doc;
  ])

let of_field ~pathloc { TypeDecl.Field.id; doc; type_; mutable_ } =
  let id = Identifier.any id in
  let name = Identifier.name id in
  let doc = maybe_doc ~pathloc doc in
  let thtml = of_type_expr ~pathloc type_ in
  let tree = BlueTree.(of_kv [
    "name", of_string name;
    "type", thtml;
    "doc",  doc;
  ]) in
  region ~pathloc id
    (if mutable_ then BlueTree.(add "mutable" (empty ()) tree) else tree)

let of_type_representation ~pathloc = TypeDecl.Representation.(BlueTree.(function
  | Variant constrs ->
    of_cons "variant" (of_list (List.map (of_constructor ~pathloc) constrs))
  | Record fields ->
    of_cons "record"  (of_list (List.map (of_field ~pathloc) fields))
  | Extensible -> of_kv [ tag "extensible" ]
))

(* TODO: Do we need to special case Var "_"? *)
let of_type_param = TypeDecl.(BlueTree.(function
  | Any, None     -> of_kv [ tag "any" ]
  | Any, Some Pos -> of_kv [ tag "any"; tag "pos" ]
  | Any, Some Neg -> of_kv [ tag "any"; tag "neg" ]
  | Var p, None     -> of_kv [ "var", of_string p ]
  | Var p, Some Pos -> of_kv [ "var", of_string p; tag "pos" ]
  | Var p, Some Neg -> of_kv [ "var", of_string p; tag "neg" ]
))

let of_type_params ps = BlueTree.of_list (List.map of_type_param ps)

let of_type ~pathloc
    { TypeDecl.id; doc;
      equation={ TypeDecl.Equation.params; private_; manifest; constraints };
      representation;
    } =
  let of_type_expr = of_type_expr ~pathloc in
  let of_rep = of_type_representation ~pathloc in
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let params = of_type_params params in
  let constraints = BlueTree.(
    of_list (List.map (fun (c,c') -> of_kv [
      "lhs", of_type_expr c;
      "rhs", of_type_expr c';
    ]) constraints)
  ) in
  let tree = BlueTree.(of_kv [
    "name", of_string name;
    "params", params;
    "constraints", constraints;
    "doc",  doc;
  ]) in
  let tree = match manifest with
    | None -> tree
    | Some t -> BlueTree.add "manifest" (of_type_expr t) tree
  in
  let tree = match representation with
    | None -> tree
    | Some r -> BlueTree.add "rep" (of_rep r) tree
  in
  let tree =
    if private_ then BlueTree.(add "private" (empty ()) tree) else tree
  in
  region ~pathloc id tree

let of_type_ext ~pathloc
    { Extension.type_path; doc; type_params; private_; constructors } =
  let doc = maybe_doc ~pathloc doc in
  let name_link = link_path ~pathloc (Path.any type_path) in
  let params = of_type_params type_params in
  let constrs = List.map (of_extension ~pathloc) constructors in
  let tree = BlueTree.(of_kv [
    "name",    of_list name_link;
    "params",  params;
    "constrs", of_list constrs;
    "doc",     doc;
  ]) in
  if private_ then BlueTree.(add "private" (empty ()) tree) else tree

let of_exception ~pathloc { Exception.id; doc; args; res } =
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let args = args_of_constructor ~pathloc args res in
  region ~pathloc id BlueTree.(of_kv [
    "name", of_string name;
    "args", args;
    "doc",  doc;
  ])

let of_instance_variable ~pathloc
    { InstanceVariable.id; doc; mutable_; virtual_; type_ } =
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let type_ = of_type_expr ~pathloc type_ in
  let tree = BlueTree.(of_kv [
    "name", of_string name;
    "type", type_;
    "doc",  doc;
  ]) in
  let tree =
    if mutable_ then BlueTree.(add "mutable" (empty ()) tree) else tree
  in
  let tree =
    if virtual_ then BlueTree.(add "virtual" (empty ()) tree) else tree
  in
  region ~pathloc id tree

let of_method ~pathloc { Method.id; doc; private_; virtual_; type_ } =
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let type_ = of_type_expr ~pathloc type_ in
  let tree = BlueTree.(of_kv [
    "name", of_string name;
    "type", type_;
    "doc",  doc;
  ]) in
  let tree =
    if private_ then BlueTree.(add "private" (empty ()) tree) else tree
  in
  let tree =
    if virtual_ then BlueTree.(add "virtual" (empty ()) tree) else tree
  in
  region ~pathloc id tree

let rec fold_doc_items f ?(suppress=false) acc = function
  | [] -> List.rev acc
  | next::rest -> match f next with
    | None -> let suppress = not suppress in fold_doc_items f ~suppress acc rest
    | Some _ when suppress -> fold_doc_items f ~suppress acc rest
    | Some html -> fold_doc_items f ~suppress (html::acc) rest

let rec of_class_signature_item ~pathloc = ClassSignature.(BlueTree.(function
  | InstanceVariable instance ->
    Some (of_cons "var" (of_instance_variable ~pathloc instance))
  | Method method_ -> Some (of_cons "method" (of_method ~pathloc method_))
  | Constraint (a, b) -> Some (of_cons "constraint" (of_kv [
    "lhs", of_type_expr ~pathloc a;
    "rhs", of_type_expr ~pathloc b;
  ]))
  | Inherit class_type ->
    Some (of_cons "inherit" (of_class_type_expr ~pathloc class_type))
  | Comment (Documentation.Documentation doc) ->
    Some (of_cons "doc" (maybe_doc ~pathloc doc))
  | Comment Documentation.Stop -> None
))
and of_class_type_expr ~pathloc = ClassType.(BlueTree.(fun expr ->
  of_lazy_tree (fun () -> match expr with
    | Constr (path, []) -> of_cons "constr" (of_kv [
      "name", of_list (link_path ~pathloc (Path.any path));
    ])
    | Constr (path, args) -> of_cons "constr" (of_kv [
      "name", of_list (link_path ~pathloc (Path.any path));
      "args", of_list (List.map (of_type_expr ~pathloc) args);
    ])
    | Signature { ClassSignature.self; items } -> of_cons "sig" (of_kv_maybe [
      "self", (match self with
        | None -> None
        | Some expr -> Some (of_type_expr ~pathloc expr)
      );
      "items",
    Some (of_list (fold_doc_items (of_class_signature_item ~pathloc) [] items));
    ])
  )
))

let rec of_class_decl ~pathloc = Class.(BlueTree.(function
  | ClassType class_type_expr ->
    of_cons "type" (of_class_type_expr ~pathloc class_type_expr)
  | Arrow (label, type_, decl) ->
    of_cons "arrow" (of_kv [
      "domain", of_labeled_type_expr ~pathloc type_ label;
      "range",  of_class_decl ~pathloc decl;
    ])
))

let class_declaration ~id ~pathloc name params decl doc virtual_ =
  let id = Identifier.any id in
  let tree = BlueTree.(of_kv [
    "name", name;
    "params", params;
    "decl", decl;
    "doc", doc;
  ]) in
  let tree =
    if virtual_ then BlueTree.(add "virtual" (empty ()) tree) else tree
  in
  region ~pathloc id tree

let of_class ~pathloc { Class.id; doc; virtual_; params; type_ } =
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = BlueTree.of_string (Identifier.name id) in
  let params = of_type_params params in
  let decl = of_class_decl ~pathloc type_ in
  class_declaration ~id ~pathloc name params decl doc virtual_

let of_class_type ~pathloc { ClassType.id; doc; virtual_; params; expr } =
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = BlueTree.of_string (Identifier.name id) in
  let params = of_type_params params in
  let decl = of_class_type_expr ~pathloc expr in
  class_declaration ~id ~pathloc name params decl doc virtual_

let module_declaration ~id ~pathloc name decl doc =
  let id = Identifier.any id in
  region ~pathloc id BlueTree.(of_kv [
    "name", name;
    "decl", decl;
    "doc",  doc;
  ])

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

let rec base_of_module_type_expr ~pathloc id = ModuleType.(function
  | TypeOf (Module.Alias (Path.Resolved path)) ->
    Some (Identifier.signature_of_module (Path.Resolved.identifier path))
  | Path (Path.Resolved path) ->
    Some (Identifier.signature_of_module_type (Path.Resolved.identifier path))
  | TypeOf (Module.Alias _)
  | Path _ -> None
  | Functor _ -> None (* TODO: can with of functor ever happen? *)
  | Signature _ -> Some id
  | TypeOf (Module.ModuleType expr)
  | With (expr, _) -> base_of_module_type_expr ~pathloc id expr
)

let of_alias ~pathloc path = BlueTree.(of_cons "alias" (of_kv [
  "path", of_list (link_path ~pathloc (Path.any path));
]))

let is_module_type_substruct type_ = not (is_short_sig type_)

let is_module_expr_substruct = function
  | None -> false
  | Some type_ -> is_module_type_substruct type_

let is_module_substruct = Module.(function
  | Alias _ -> false
  | ModuleType type_ -> is_module_type_substruct type_
)

let rec of_module ~pathloc { Module.id; doc; type_ } =
  let decl =
    decl_of_decl ~pathloc (Identifier.signature_of_module id) type_
  in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let name = BlueTree.(
    if is_module_substruct type_
    then of_list (link_ident ~text:(txt name) ~pathloc () id)
    else of_string name
  ) in
  let doc = maybe_doc ~pathloc doc in
  module_declaration ~id ~pathloc name decl doc

and decl_of_decl ?top ~pathloc id = Module.(BlueTree.(function
  | Alias path -> of_alias ~pathloc path
  | ModuleType module_type ->
    of_cons "sig" (decl_of_sig ?top ~pathloc id module_type)
))

and decl_of_sig ?(top=false) ~pathloc id = ModuleType.(BlueTree.(fun sig_ ->
  of_lazy_tree (fun () -> match sig_ with
    | Path path -> of_cons "path" (of_list (link_path ~pathloc (Path.any path)))
    | Signature s -> of_kv_maybe [
      "substruct", if top then None else Some (empty ());
      "sig", Some (of_signature ~pathloc id s);
    ]
    | Functor (None, expr) ->
      of_cons "functor" (of_kv [
        "range", decl_of_sig ~top ~pathloc id expr;
      ])
    | Functor (Some (arg_ident, arg_sig), expr) ->
      let arg_decl = decl_of_sig ~top ~pathloc id arg_sig in
      let range = decl_of_sig ~top ~pathloc id expr in
      let arg = Identifier.name (Identifier.any arg_ident) in (* TODO: more? *)
      of_cons "functor" (of_kv [
        "arg", of_string arg;
        "arg-type", arg_decl;
        "range", range;
      ])
    | With (With (expr, subs), subs') ->
      decl_of_sig ~top ~pathloc id (With (expr, subs @ subs'))
    | With (expr, subs) ->
      let base = base_of_module_type_expr ~pathloc id expr in
      let decl = decl_of_sig ~top ~pathloc id expr in
      let tree = of_kv [
        "lhs", decl;
        "subs", of_substitutions ~pathloc base [] subs;
      ] in
      let tree = if is_short_sig expr then add "short" (empty ()) tree else tree in
      of_cons "with" tree
    | TypeOf (Module.Alias path) ->
      let path = Path.any path in
      of_cons "typeof" (of_cons "alias" (of_list (link_path ~pathloc path)))
    | TypeOf (Module.ModuleType module_type) ->
      let decl = decl_of_sig ~top ~pathloc id module_type in
      of_cons "typeof" (of_cons "type" decl)
  )
))

and of_substitutions ~pathloc base acc = ModuleType.(BlueTree.(function
  | (ModuleEq (module_frag, module_eqn))::subs ->
    (* TODO: This seems too wide. See doc-ock#53. *)
    let decl = begin match module_eqn with
      | Module.Alias path -> of_alias ~pathloc path
      | Module.ModuleType _ -> empty ()
    end in
    let name = link_fragment ~pathloc base (Fragment.any module_frag) in
    let tree = of_cons "module" (of_kv [
      "name", of_list name;
      "decl", decl;
    ]) in
    of_substitutions ~pathloc base (tree::acc) subs
  | (TypeEq (type_frag, type_eqn))::subs ->
    let { TypeDecl.Equation.params; private_; manifest } = type_eqn in
    let name = link_fragment ~pathloc base (Fragment.any type_frag) in
    let params = of_type_params params in
    let manifest = match manifest with
      | Some m -> Some (of_type_expr ~pathloc m)
      | None -> None
    in
    let tree = of_cons "type" (of_kv_maybe [
      "name", Some (of_list name);
      "params", Some params;
      "manifest", manifest;
      "private", if private_ then Some (empty ()) else None;
    ]) in
    of_substitutions ~pathloc base (tree::acc) subs
  | (ModuleSubst (module_frag, module_path))::subs ->
    let rhs = link_path ~pathloc (Path.any module_path) in
    let name = link_fragment ~pathloc base (Fragment.any module_frag) in
    let tree = of_cons "module-subst" (of_kv [
      "name", of_list (name);
      "subst", of_list (rhs);
    ]) in
    of_substitutions ~pathloc base (tree::acc) subs
  | (TypeSubst (type_frag, params, type_path))::subs ->
    let name = link_fragment ~pathloc base (Fragment.any type_frag) in
    let params =
      of_type_params (List.map (fun p -> (TypeDecl.Var p, None)) params)
    in
    let type_path = link_path ~pathloc (Path.any type_path) in
    let tree = of_cons "type-subst" (of_kv [
      "name", of_list (name);
      "params", params;
      "subst", of_list (type_path);
    ]) in
    of_substitutions ~pathloc base (tree::acc) subs
  | [] -> of_list (List.rev acc)
))

and of_module_type ~pathloc { ModuleType.id; doc; expr } =
  let doc = maybe_doc ~pathloc doc in
  let decl = BlueTree.(match expr with
    | None -> empty ()
    | Some expr ->
      decl_of_sig ~pathloc (Identifier.signature_of_module_type id) expr
  ) in
  let id = Identifier.any id in
  let name = Identifier.name id in
  let name = BlueTree.(
    if is_module_expr_substruct expr
    then of_list (link_ident ~text:(txt name) ~pathloc () id)
    else of_string name
  ) in
  module_declaration ~id ~pathloc name decl doc

and of_signature_item ~pathloc id = Signature.(BlueTree.(function
  | Value val_ -> Some (of_cons "val" (of_value ~pathloc val_))
  | External ext -> Some (of_cons "external" (of_external ~pathloc ext))
  | Type type_ -> Some (of_cons "type" (of_type ~pathloc type_))
  | TypExt ext -> Some (of_cons "type-ext" (of_type_ext ~pathloc ext))
  | Exception exn -> Some (of_cons "exn" (of_exception ~pathloc exn))
  | Class class_ -> Some (of_cons "class" (of_class ~pathloc class_))
  | ClassType class_type ->
    Some (of_cons "class-type" (of_class_type ~pathloc class_type))
  | Module module_ -> Some (of_cons "module" (of_module ~pathloc module_))
  | ModuleType module_type ->
    Some (of_cons "module-type" (of_module_type ~pathloc module_type))
  | Include { Include.expr } ->
    Some (of_cons "include" (decl_of_sig ~top:true ~pathloc id expr))
  | Comment (Documentation.Documentation doc) ->
    Some (of_cons "doc" (maybe_doc ~pathloc doc))
  | Comment Documentation.Stop -> None
))

and of_signature ~pathloc id signature =
  BlueTree.of_list (fold_doc_items (of_signature_item ~pathloc id) [] signature)

let add_up ~pathloc tree = match CodocUnit.Href.up pathloc with
  | Some up_href ->
    let up_href = Uri.to_string up_href in
    BlueTree.(add "up" (of_cons "href" (of_string up_href)) tree)
  | None -> tree

let of_top_module ~loc { Module.id; doc; type_ } =
  let pathloc = loc in
  let doc = maybe_doc ~pathloc doc in
  let top = true in
  let decl =
    decl_of_decl ~top ~pathloc (Identifier.signature_of_module id) type_
  in
  let id = Identifier.any id in
  let name = link_ident ~pathloc () id in
  let tree = module_declaration ~id ~pathloc (BlueTree.of_list name) decl doc in
  BlueTree.(root (add_up ~pathloc (of_cons "module" tree)))

let of_top_moduletype ~loc = function
  | { ModuleType.expr = None } -> BlueTree.empty () (* TODO: ??? *)
  | { ModuleType.id; doc; expr = Some expr } ->
    let pathloc = loc in
    let doc = maybe_doc ~pathloc doc in
    let top = true in
    let decl =
      decl_of_sig ~top ~pathloc (Identifier.signature_of_module_type id) expr
    in
    let id = Identifier.any id in
    let name = link_ident ~pathloc () id in
    let tree = module_declaration ~id ~pathloc (BlueTree.of_list name) decl doc in
    BlueTree.(root (add_up ~pathloc (of_cons "module-type" tree)))

let of_top_class ~loc { Class.id; doc; virtual_; params; type_ } =
  let pathloc = loc in
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = BlueTree.of_list (link_ident ~pathloc () id) in
  let params = of_type_params params in
  let decl = of_class_decl ~pathloc type_ in
  let tree = class_declaration ~id ~pathloc name params decl doc virtual_ in
  BlueTree.(root (add_up ~pathloc (of_cons "class" tree)))

let of_top_classtype ~loc { ClassType.id; doc; virtual_; params; expr } =
  let pathloc = loc in
  let doc = maybe_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = BlueTree.of_list (link_ident ~pathloc () id) in
  let params = of_type_params params in
  let decl = of_class_type_expr ~pathloc expr in
  let tree = class_declaration ~id ~pathloc name params decl doc virtual_ in
  BlueTree.(root (add_up ~pathloc (of_cons "class-type" tree)))
