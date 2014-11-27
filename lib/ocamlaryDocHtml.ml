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

type root = OcamlaryDoc.root =
            | Cmti of string * string
            | Xml of string * root
            | Html of string * root

type pathloc = {
  index_depth   : int;
  doc_base      : Uri.t;
  unit_depth    : int;
  internal_path : root Identifier.signature;
}

module Identifier = struct
  include Identifier

  type signature_kind = [ `Module | `ModuleType ]
  type class_signature_kind = [ `Class | `ClassType ]
  type container_kind = [ signature_kind | class_signature_kind ]
  type parent_kind = [ container_kind | `Type ]

  type 'a parent = ('a, parent_kind) Identifier.t

  class type ['a] module_map = object
    method root : root -> 'a
    method argument : root signature -> int -> string -> 'a
    method module_ : root signature -> string -> 'a
  end

  class type ['a] module_type_map = object
    method module_type : root signature -> string -> 'a
  end

  class type ['a] type_map = object
    method type_ : root signature -> string -> 'a
    method core_type : string -> 'a
  end

  class type ['a] constructor_map = object
    method constructor : root type_ -> string -> 'a
  end

  class type ['a] field_map = object
    method field : root type_ -> string -> 'a
  end

  class type ['a] extension_map = object
    method extension : root signature -> string -> 'a
  end

  class type ['a] exception_map = object
    method exception_ : root signature -> string -> 'a
    method core_exception : string -> 'a
  end

  class type ['a] value_map = object
    method value : root signature -> string -> 'a
  end

  class type ['a] class_map = object
    method class_ : root signature -> string -> 'a
  end

  class type ['a] class_type_map = object
    method class_type : root signature -> string -> 'a
  end

  class type ['a] method_map = object
    method method_ : root class_signature -> string -> 'a
  end

  class type ['a] instance_variable_map = object
    method instance_variable : root class_signature -> string -> 'a
  end

  class type ['a] label_map = object
    method label : root container -> string -> 'a
  end

  class type ['a] signature_map = object
    inherit ['a] module_map
    inherit ['a] module_type_map
  end

  class type ['a] class_signature_map = object
    inherit ['a] class_map
    inherit ['a] class_type_map
  end

  class type ['a] container_map = object
    inherit ['a] signature_map
    inherit ['a] class_signature_map
  end

  class type ['a] any_map = object
    inherit ['a] container_map
    inherit ['a] type_map
    inherit ['a] constructor_map
    inherit ['a] field_map
    inherit ['a] extension_map
    inherit ['a] exception_map
    inherit ['a] value_map
    inherit ['a] method_map
    inherit ['a] instance_variable_map
    inherit ['a] label_map
  end

  let parent_of_signature : 'a signature -> 'a parent = function
    | Root _ | Module _ | Argument _ | ModuleType _ as x -> x

  let parent_of_type : 'a type_ -> 'a parent = function
    | Type _ | CoreType _ as x -> x

  let parent_of_class_signature : 'a class_signature -> 'a parent = function
    | Class _ | ClassType _ as x -> x

  let parent_of_container : 'a container -> 'a parent = function
    | Root _ | Module _ | Argument _ | ModuleType _
    | Class _ | ClassType _ as x -> x

  class virtual ['a] parent_map = object (self)
    method private virtual parent :
        root any -> root parent -> string -> 'a

    method virtual root : root -> 'a
    method virtual argument : root signature -> int -> string -> 'a
    method virtual core_type : string -> 'a
    method virtual core_exception : string -> 'a

    method module_ p n =
      self#parent (Module (p, n)) (parent_of_signature p) n
    method module_type p n =
      self#parent (ModuleType (p, n)) (parent_of_signature p) n
    method type_ p n =
      self#parent (Type (p, n)) (parent_of_signature p) n
    method constructor p n =
      self#parent (Constructor (p, n)) (parent_of_type p) n
    method field p n =
      self#parent (Field (p, n)) (parent_of_type p) n
    method extension p n =
      self#parent (Extension (p, n)) (parent_of_signature p) n
    method exception_ p n =
      self#parent (Exception (p, n)) (parent_of_signature p) n
    method value p n =
      self#parent (Value (p, n)) (parent_of_signature p) n
    method class_ p n =
      self#parent (Class (p, n)) (parent_of_signature p) n
    method class_type p n =
      self#parent (ClassType (p, n)) (parent_of_signature p) n
    method method_ p n =
      self#parent (Method (p, n)) (parent_of_class_signature p) n
    method instance_variable p n =
      self#parent (InstanceVariable (p, n)) (parent_of_class_signature p) n
    method label p n =
      self#parent (Label (p, n)) (parent_of_container p) n
  end
end

let map_ident map = Identifier.(function
  | Root root                  -> map#root root
  | CoreType name              -> map#core_type name
  | CoreException name         -> map#core_exception name
  | Argument (parent, i, name) -> map#argument parent i name
  | Module (p, name)           -> map#module_ p name
  | ModuleType (p, name)       -> map#module_type p name
  | Type (p, name)             -> map#type_ p name
  | Constructor (p, name)      -> map#constructor p name
  | Field (p, name)            -> map#field p name
  | Extension (p, name)        -> map#extension p name
  | Exception (p, name)        -> map#exception_ p name
  | Value (p, name)            -> map#value p name
  | Class (p, name)            -> map#class_ p name
  | ClassType (p, name)        -> map#class_type p name
  | Method (p, name)           -> map#method_ p name
  | InstanceVariable (p, name) -> map#instance_variable p name
  | Label (p, name)            -> map#label p name
)

module Fragment_resolved = struct
  include Fragment.Resolved

  type type_kind = [ `Type | `Class | `ClassType ]

  class type ['a] root_map = object
    method root : 'a
  end

  class type ['a] module_map = object
    method module_ : signature -> string -> 'a
  end

  class type ['a] signature_map = object
    inherit ['a] root_map
    inherit ['a] module_map
  end

  class type ['a] type_map = object
    method type_ : signature -> string -> 'a
  end

  class type ['a] class_map = object
    method class_ : signature -> string -> 'a
  end

  class type ['a] class_type_map = object
    method class_type : signature -> string -> 'a
  end

  class type ['a] type_kind_map = object
    inherit ['a] type_map
    inherit ['a] class_map
    inherit ['a] class_type_map
  end

  class type ['a] any_map = object
    inherit ['a] module_map
    inherit ['a] type_kind_map
  end

  class type ['a] raw_map = object
    inherit ['a] root_map
    inherit ['a] any_map
  end

  class virtual ['a] parent_map = object (self)
    method private virtual parent : any -> signature -> string -> 'a

    method module_ p n    = self#parent (Module (p, n)) p n
    method type_ p n      = self#parent (Type (p, n)) p n
    method class_ p n     = self#parent (Class (p, n)) p n
    method class_type p n = self#parent (ClassType (p, n)) p n
  end
end

let map_resolved_fragment
    : 'a #Fragment_resolved.any_map -> Fragment_resolved.any -> 'a
  = fun map ->
    Fragment_resolved.(function
    | Module (p, name)    -> map#module_ p name
    | Type (p, name)      -> map#type_ p name
    | Class (p, name)     -> map#class_ p name
    | ClassType (p, name) -> map#class_type p name
    )

let parent_list ident =
  let rec path acc ident = Identifier.(match ident with
    | Root _ -> ident::acc
    | Module (p, _)
    | Argument (p, _, _)
    | ModuleType (p, _) -> path (ident::acc) p
  ) in
  List.(tl (rev (path [] ident)))

let pathloc ~index_depth ~doc_base internal_path =
  {
    index_depth; doc_base;
    unit_depth = List.length (parent_list internal_path);
    internal_path;
  }

let keyword text = <:html<<span class="keyword">$str:text$</span>&>>

let rec ascent_of_depth tl = function
  | 0 -> tl
  | n -> ascent_of_depth ("../" ^ tl) (n - 1)

let href_of_project ~pathloc project =
  Uri.of_string ((ascent_of_depth "" pathloc.index_depth) ^ project ^ "/")

class name_of_ident_map : [string] Identifier.any_map = object (self)
  inherit [string] Identifier.parent_map

  method root = OcamlaryDoc.name_of_root
  method core_type name = name
  method core_exception name = name
  method argument _ _ name = name
  method private parent _ _ name = name
end

let name_of_ident_map = new name_of_ident_map

let name_of_ident = map_ident name_of_ident_map

let type_class = "type"
let exn_class = "exn"

(* TODO: use these for CSS class names? *)
let ident_class = Identifier.(function
  | Root _ -> "root"
  | Module _ -> "module"
  | Argument _ -> "modarg"
  | ModuleType _ -> "modtype"
  | Type _ -> type_class
  | CoreType _ -> type_class
  | Constructor _ -> "cons"
  | Field _ -> "field"
  | Extension _ -> "ext"
  | Exception _ -> exn_class
  | CoreException _ -> exn_class
  | Value _ -> "val"
  | Class _ -> "class"
  | ClassType _ -> "classtype"
  | Method _ -> "method"
  | InstanceVariable _ -> "var"
  | Label _ -> "label"
)

let classify cls name = Printf.sprintf "%s:%s" cls name

class id_of_ident_map ~pathloc : [string] Identifier.any_map = object (self)
  inherit [string] Identifier.parent_map

  method root _ = "" (* TODO: check against pathloc; should be self *)
  method core_type name = classify type_class name
  method core_exception name = classify exn_class name
  method argument parent i name = (* TODO: fixme *)
    self#parent
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      name
  method private parent ident parent name =
    Printf.sprintf "%s/%s"
      (map_ident self (Identifier.any parent))
      (classify (ident_class ident) name)
end

let id_of_ident_map ~pathloc = new id_of_ident_map ~pathloc

let id_of_ident ~pathloc = map_ident (id_of_ident_map ~pathloc)

class href_of_ident_map ~pathloc : [Uri.t] Identifier.any_map = object (self)
  inherit [Uri.t] Identifier.parent_map

  method root r = (* TODO: fixme with pathloc *)
    Uri.of_string (OcamlaryDoc.path_of_root r)
  method core_type name = Uri.of_string name (* TODO: fixme *)
  method core_exception name = Uri.of_string name (* TODO: fixme *)
  method argument parent i name = (* TODO: fixme *)
    self#parent
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      name
  method private parent ident parent name = (* TODO: fixme with pathloc *)
    Uri.(with_fragment (of_string "") (Some (id_of_ident ~pathloc ident)))
end

let href_of_ident_map ~pathloc = new href_of_ident_map ~pathloc

let href_of_ident ~pathloc : 'a Identifier.any -> Uri.t =
  map_ident (href_of_ident_map ~pathloc)

class link_ident_map ?text ~pathloc () : [Cow.Html.t] Identifier.any_map =
object (self)
  inherit [Cow.Html.t] Identifier.parent_map

  method root r =
    let uri = Uri.of_string (OcamlaryDoc.path_of_root r) in
    <:html<<a href=$uri:uri$>$str:OcamlaryDoc.name_of_root r$</a>&>>
  method core_type name = <:html<$str:name$>> (* TODO: link *)
  method core_exception name = <:html<$str:name$>> (* TODO: link *)
  method argument parent i name = (* TODO: fixme *)
    self#parent
      (Identifier.Argument (parent, i, name))
      (Identifier.parent_of_signature parent)
      name
  method private parent ident parent name =
    let href = href_of_ident ~pathloc ident in
    match text with
    | Some html ->
      <:html<<a href=$uri:href$>$html$</a>&>>
    | None ->
      if parent = (Identifier.parent_of_signature pathloc.internal_path)
      then <:html<<a href=$uri:href$>$str:name$</a>&>>
      else let phtml = map_ident self (Identifier.any parent) in
           <:html<$phtml$.<a href=$uri:href$>$str:name$</a>&>>
end

let rec link_path ~pathloc : ('a,'b) Path.t -> Cow.Html.t =
  Path.(function
  | Root root -> <:html<$str:root$>>
  | Dot (parent, name) ->
    <:html<$link_path ~pathloc (any parent)$.$str:name$>>
  | Resolved resolved ->
    let link_ident_map = new link_ident_map ~pathloc () in
    map_ident link_ident_map (Identifier.any (Resolved.identifier resolved))
  | Apply (m, m') -> (* TODO: test *)
    <:html<$link_path ~pathloc (any m)$($link_path ~pathloc (any m')$)>>
  )

class link_resolved_fragment_map ~pathloc base
  : [Cow.Html.t] Fragment_resolved.any_map =
object (self)
  inherit [Cow.Html.t] Fragment_resolved.parent_map

  method private parent fragment parent name = Fragment_resolved.(
    let text = <:html<$str:name$>> in
    let link = match base with
      | Some base ->
        let ident = Identifier.any (identifier base fragment) in
        map_ident (new link_ident_map ~text ~pathloc ()) ident
      | None -> text
    in
    match parent with
    | Root -> link
    | Module _ as parent -> (* need to capture GADT eq *)
      let phtml = map_resolved_fragment self parent in
      <:html<$phtml$.$link$>>
  )
end

let link_resolved_fragment_map ~pathloc base =
  new link_resolved_fragment_map ~pathloc base

let link_resolved_fragment ~pathloc base =
  map_resolved_fragment (link_resolved_fragment_map ~pathloc base)

let rec link_fragment ~pathloc base : 'a Fragment.t -> Cow.Html.t =
  Fragment.(function
  | Resolved resolved -> link_resolved_fragment ~pathloc base resolved
  | Dot (Resolved Resolved.Root, name) -> <:html<$str:name$>> (* TODO: test *)
  | Dot ((Dot _ | Resolved (Resolved.Module _)) as parent , name) ->
    <:html<$link_fragment ~pathloc base parent$.$str:name$>>
  )

let rec link_reference ?text ~pathloc : ('a,'b) Reference.t -> Cow.Html.t =
  Reference.(function
  | Root root -> <:html<[root:$str:root$]>> (* TODO: test wtf *)
  | Dot (parent, name) ->
    begin match text with
    | None -> <:html<$link_reference ~pathloc (any parent)$.$str:name$>>
    | Some html -> html (* TODO: link ?? *)
    end
  | Resolved resolved ->
    let link_ident_map = new link_ident_map ?text ~pathloc () in
    map_ident link_ident_map (Resolved.identifier resolved)
  )

let section_attrs ?level ~pathloc label_opt =
  (* TODO: ids for unnamed sections? ocamldoc does it... *)
  let level_attrs = match level with
    | None       -> ["class","section"]
    | Some level -> ["class","section level_"^(string_of_int level)]
  in
  let label_attrs = match label_opt with
    | None -> []
    | Some label -> ["id",id_of_ident ~pathloc (Identifier.any label)]
  in
  level_attrs@label_attrs

let rec of_text_element ~pathloc txt =
  let of_text_elements = of_text_elements ~pathloc in
  Reference.(Documentation.(match txt with
  | Raw s -> <:html<$str:s$>>
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
    <:html<
    <h1 $alist:section_attrs ~pathloc label_opt$>$of_text_elements els$</h1>
    >>
  | Title (2,label_opt,els) ->
    <:html<
    <h2 $alist:section_attrs ~pathloc label_opt$>$of_text_elements els$</h2>
    >>
  | Title (3,label_opt,els) ->
    <:html<
    <h3 $alist:section_attrs ~pathloc label_opt$>$of_text_elements els$</h3>
    >>
  | Title (4,label_opt,els) ->
    <:html<
    <h4 $alist:section_attrs ~pathloc label_opt$>$of_text_elements els$</h4>
    >>
  | Title (5,label_opt,els) ->
    <:html<
    <h5 $alist:section_attrs ~pathloc label_opt$>$of_text_elements els$</h5>
    >>
  | Title (6,label_opt,els) ->
    <:html<
    <h6 $alist:section_attrs ~pathloc label_opt$>$of_text_elements els$</h6>
    >>
  | Title (level,label_opt,els) when level < 1 ->
    <:html<
    <h1 $alist:section_attrs ~level ~pathloc label_opt$>$of_text_elements els$</h1>
    >>
  | Title (level,label_opt,els) ->
    <:html<
    <h6 $alist:section_attrs ~level ~pathloc label_opt$>$of_text_elements els$</h6>
    >>
  | Reference (Module m, None) -> link_reference ~pathloc (any m)
  | Reference (Module m, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any m)
  | Reference (ModuleType m, None) -> link_reference ~pathloc (any m)
  | Reference (ModuleType m, Some els) -> (* TODO: test *)
    link_reference ~text:<:html<$of_text_elements els$>> ~pathloc (any m)
  | Reference (Type t, None) -> link_reference ~pathloc (any t)
  | Reference (Type t, Some els) -> (* TODO: test *)
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
  | Special (Modules mods) -> (* TODO: test *)
    let mods_html = List.map (fun m -> link_reference ~pathloc (any m)) mods in
    <:html<<span style="color: red">
           Special module index: $list:mods_html$
           </span>
    >>
  | Special Index -> (* TODO: test *)
    <:html<<span style="color: red">
           Special index
           </span>
    >>
  ))
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
        $of_text_elements ~pathloc text$
        $list:List.map (map_tag ~pathloc div_tag) tags$
      </div>
    >>

let maybe_span_doc ~pathloc doc = Documentation.(match doc.text with
  | [] -> <:html<&>>
  | text ->
    let tags = List.map (map_tag ~pathloc span_tag) doc.tags in
    let comment = of_text_elements ~pathloc text in
    <:html< <span class="doc">(* $comment$$list:tags$ *)</span>&>>
)

let fold_html sep = List.fold_left (fun phtml ehtml ->
  <:html<$phtml$$sep$$ehtml$>>
)

let fold_html_str sep = fold_html <:html<$str:sep$>>

let rec of_type_expr ~pathloc expr =
  let of_type_expr = of_type_expr ~pathloc in
  TypeExpr.(match expr with
  | Var v when v = "_" -> <:html<_>>
  | Var v -> <:html<'$str:v$>>
  | Any -> <:html<<span style="color: red">TODO: ANY</span>&>> (* TODO: test *)
  | Alias (t,v) -> (* TODO: parens are only sometimes required *)
    <:html<($of_type_expr t$ $keyword "as"$ '$str:v$)&>>
  | Arrow (label, t, t') ->
    <:html<$of_labeled_type_expr ~pathloc t label$ -> $of_type_expr t'$>>
  | Tuple []       -> <:html<()>>
  | Tuple (e::els) ->
    let e = of_type_expr e in
    let els = List.map of_type_expr els in
    <:html<($fold_html_str " * " e els$)>>
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
  | Package { Package.path; substitutions } -> (* TODO: test *)
    (* TODO: substitutions *)
    <:html<(module $link_path ~pathloc (Path.any path)$)>>
  )
and of_object_method ~pathloc { TypeExpr.Object.name; type_ } =
  <:html<$str:name$ : $of_type_expr ~pathloc type_$&>>
and of_type_constr ?(cons="") ~pathloc path = function
  | []  -> link_path ~pathloc (Path.any path)
  | [a] ->
    let arg = of_type_expr ~pathloc a in
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
    <:html<`$str:name$ of $arghtml$&>>
  )
and polyvar_elements ~pathloc = List.map (fun pve ->
  <:html<<div class="cons">| $polyvar_element ~pathloc pve$</div>&>>
)
and of_labeled_type_expr ~pathloc t = TypeExpr.(function
  | None -> of_type_expr ~pathloc t
  | Some (Label l) -> <:html<$str:l$:$of_type_expr ~pathloc t$>>
  | Some (Optional l) -> <:html<?$str:l$:$of_type_expr ~pathloc t$>>
)

let of_value ~pathloc { Value.id; doc; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let ident = Identifier.any id in
  let name = name_of_ident ident in
  let id = id_of_ident ~pathloc ident in
  <:html<
  <div id=$str:id$>
  <div class="val">
    $keyword "val"$ $str:name$ : $of_type_expr ~pathloc type_$
    $doc$
  </div>
  </div>
  >>

let of_external ~pathloc { External.id; doc; type_; primitives } =
  let doc = maybe_div_doc ~pathloc doc in
  let ident = Identifier.any id in
  let name = name_of_ident ident in
  let id = id_of_ident ~pathloc ident in
  let primitives = List.map (fun p -> <:html<"$str:p$" >>) primitives in
  <:html<
  <div id=$str:id$>
  <div class="external val">
    $keyword "external"$ $str:name$ : $of_type_expr ~pathloc type_$
    = $list:primitives$
    $doc$
  </div>
  </div>
  >>

let args_of_constructor ~pathloc args res =
  let of_type_expr = of_type_expr ~pathloc in
  match args, res with
  | [],       None    -> <:html<&>>
  | a::args,  None    ->
    let a = of_type_expr a in
    let args = List.map of_type_expr args in
    let arghtml = fold_html_str " * " a args in
    <:html< $keyword "of"$ $arghtml$>>
  | [],       Some rt -> <:html< : $of_type_expr rt$>>
  | a::args,  Some rt ->
    let a = of_type_expr a in
    let args = List.map of_type_expr args in
    let arghtml = fold_html_str " * " a args in
    <:html< : $arghtml$ -> $of_type_expr rt$>>

(* TODO: add id *)
let of_constructor ~pathloc { TypeDecl.Constructor.id; doc; args; res } =
  let name = name_of_ident (Identifier.any id) in
  let sig_ = args_of_constructor ~pathloc args res in
  let doc  = maybe_span_doc ~pathloc doc in
  <:html<<div class="cons">| $str:name$$sig_$$doc$</div>&>>

(* TODO: add id *)
let of_extension ~pathloc { Extension.Constructor.id; doc; args; res } =
  let name = name_of_ident (Identifier.any id) in
  let sig_ = args_of_constructor ~pathloc args res in
  let doc  = maybe_span_doc ~pathloc doc in
  <:html<<div class="cons">| $str:name$$sig_$$doc$</div>&>>

(* TODO: add id *)
let of_field ~pathloc { TypeDecl.Field.id; doc; type_ } =
  let name = name_of_ident (Identifier.any id) in
  let doc = maybe_span_doc ~pathloc doc in
  let thtml = of_type_expr ~pathloc type_ in
  <:html<<div class="field">$str:name$ : $thtml$;$doc$</div>&>>

let of_type_representation ~pathloc = TypeDecl.Representation.(function
  | Variant constrs ->
    <:html<$list:List.map (of_constructor ~pathloc) constrs$>>
  | Record fields -> <:html<{$list:List.map (of_field ~pathloc) fields$}>>
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
      <:html<$str:string_of_variance vari$<span style="color: red">ANY</span> >> (* TODO: test *)
    | [Var p, vari] -> <:html<$str:string_of_variance vari$'$str:p$ >>
    | ps            ->
      let type_vars = List.map (function
        | Any,   vari -> string_of_variance vari ^ "_"
        | Var v, vari -> string_of_variance vari ^ "'"^v
      ) ps in
      <:html<($str:String.concat ", " type_vars$) >>
)

(* TODO: private and constraints *)
let of_type ~pathloc
    { TypeDecl.id; doc;
      equation={ TypeDecl.Equation.params; private_; manifest; constraints };
      representation;
    } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let params = of_type_params params in
  let manifest = match manifest with
    | None -> <:html<&>>
    | Some t -> <:html< = $of_type_expr ~pathloc t$>>
  in
  let representation = match representation with
    | None -> <:html<&>>
    | Some r -> <:html< = $of_type_representation ~pathloc r$>>
  in
  let id = id_of_ident ~pathloc id in
  <:html<
  <div id=$str:id$>
  <div class="type">
    $keyword "type"$ $params$$str:name$$manifest$$representation$
    $doc$
  </div>
  </div>
  >>

(* TODO: test shadowed ext constructors *)
(* TODO: test type ext path *)
let of_type_ext ~pathloc
    { Extension.type_path; doc; type_params; private_; constructors } =
  let doc = maybe_div_doc ~pathloc doc in
  let name_link = link_path ~pathloc (Path.any type_path) in
  let params = of_type_params type_params in
  let private_ = if private_ then keyword "private" else <:html<&>> in
  let constrs = List.map (of_extension ~pathloc) constructors in
  <:html<
  <div class="ext">
    $keyword "type"$ $params$$name_link$ += $private_$$list:constrs$
    $doc$
  </div>
  >>

let of_exception ~pathloc { Exception.id; doc; args; res } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let args = args_of_constructor ~pathloc args res in
  let id = id_of_ident ~pathloc id in
  <:html<
  <div id=$str:id$>
  <div class="exn">
    $keyword "exception"$ $str:name$$args$
    $doc$
  </div>
  </div>
  >>

let of_comment ~pathloc = Documentation.(function
  | Documentation doc -> maybe_div_doc ~pathloc doc
  | Stop -> <:html<<span style="color: red">STOP</span>&>> (* TODO: do *)
)

let of_instance_variable ~pathloc
    { InstanceVariable.id; doc; mutable_; virtual_; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let id = id_of_ident ~pathloc id in
  let mutable_ =
    if mutable_ then <:html< $keyword "mutable"$>> else <:html<&>>
  in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let type_ = of_type_expr ~pathloc type_ in
  <:html<
  <div id=$str:id$>
  <div class="classval">
  $keyword "val"$$mutable_$$virtual_$ $str:name$ : $type_$
  $doc$
  </div>
  </div>
  >>

let of_method ~pathloc { Method.id; doc; private_; virtual_; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let id = id_of_ident ~pathloc id in
  let private_ =
    if private_ then <:html< $keyword "private"$>> else <:html<&>>
  in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let type_ = of_type_expr ~pathloc type_ in
  <:html<
  <div id=$str:id$>
  <div class="method">
  $keyword "method"$$private_$$virtual_$ $str:name$ : $type_$
  $doc$
  </div>
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

let rec of_class_signature_item ~pathloc = ClassSignature.(function
  | InstanceVariable instance -> of_instance_variable ~pathloc instance
  | Method method_ -> of_method ~pathloc method_
  | Constraint (a, b) ->
    <:html<
    $keyword "constraint"$ $of_type_expr ~pathloc a$ $of_type_expr ~pathloc b$
    >>
  | Inherit class_type ->
    <:html<$keyword "inherit"$ $of_class_type_expr ~pathloc class_type$>>
  | Comment comment -> of_comment ~pathloc comment
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
    <:html<$self$$list:List.map (of_class_signature_item ~pathloc) items$>>
)

let rec of_class_decl ~pathloc = Class.(function
  | ClassType class_type_expr -> of_class_type_expr ~pathloc class_type_expr
  | Arrow (label, type_, decl) ->
    <:html<
    $of_labeled_type_expr ~pathloc type_ label$ -> $of_class_decl ~pathloc decl$
    >>
)

let of_class ~pathloc { Class.id; doc; virtual_; params; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let params = of_class_params params in
  let decl = of_class_decl ~pathloc type_ in
  let id = id_of_ident ~pathloc id in
  <:html<
  <div id=$str:id$>
  <div class="class">
    $keyword "class"$$virtual_$$params$ $str:name$ : $decl$
    $doc$
  </div>
  </div>
  >>

let of_class_type ~pathloc { ClassType.id; doc; virtual_; params; expr } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let virtual_ =
    if virtual_ then <:html< $keyword "virtual"$>> else <:html<&>>
  in
  let params = of_class_params params in
  let expr = of_class_type_expr ~pathloc expr in
  let id = id_of_ident ~pathloc id in
  <:html<
  <div id=$str:id$>
  <div class="classtype">
    $keyword "class type"$$virtual_$$params$ $str:name$ = $expr$
    $doc$
  </div>
  </div>
  >>

let module_declaration
    ?(extra_classes=[]) ?(title_fn=(fun x -> x)) ~id ~pathloc
    name rhs doc rest =
  let classes = String.concat " " ("module"::extra_classes) in
  let title = title_fn <:html<$keyword "module"$ $name$>> in
  let id = Identifier.any id in
  let id = id_of_ident ~pathloc id in
  <:html<
  <div id=$str:id$>
    <div class=$str:classes$>
      <div class="intro">$title$ $rhs$</div>
      $doc$
      $rest$
    </div>
  </div>
  >>

let module_type_declaration ~id name rhs doc rest =
  <:html<
    <div id=$str:id$>
    <div class="modtype">
      <div class="intro">
        $keyword "module type"$ $str:name$ $rhs$
      </div>
      $doc$
      $rest$
    </div>
    </div>
  >>

(* Predicate for module type expressions that don't have a signature body
   in the primary expression. *)
let rec is_short_sig = ModuleType.(function
  | Ident _ -> true
  | Signature _ -> false
  | Functor (_, expr) -> is_short_sig expr
  | With (expr, _) -> is_short_sig expr
  | TypeOf (Module.Alias _) -> true
  | TypeOf (Module.ModuleType expr) -> is_short_sig expr
)

let rec base_of_module_type_expr ~pathloc = ModuleType.(function
  | TypeOf (Module.Alias (Path.Resolved path)) ->
    Some (Identifier.module_signature (Path.Resolved.identifier path))
  | Ident (Path.Resolved path) ->
    Some (Identifier.module_type_signature (Path.Resolved.identifier path))
  | TypeOf (Module.Alias _)
  | Ident _ -> None
  | Signature _ | Functor _ -> (* TODO: make sure anonymous paths are named *)
    Some pathloc.internal_path
  | TypeOf (Module.ModuleType expr)
  | With (expr, _) -> base_of_module_type_expr ~pathloc expr
)

let rec of_module ~pathloc { Module.id; doc; type_ } =
  let pathloc = { pathloc with
    unit_depth = pathloc.unit_depth + 1;
    internal_path = Identifier.module_signature id;
  } in
  let id = Identifier.any id in
  let name = name_of_ident id in
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
  | Ident path -> link_path ~pathloc (Path.any path), <:html<&>>
  | Signature s -> keyword "sig",
    let signature = of_signature ~pathloc s in
    <:html<
    <div class="sig">$signature$</div>
    <div class="outro">$keyword "end"$</div>
    >>
  | Functor (None, expr) -> (* TODO: test *)
    let rhs, rest = rhs_rest_of_sig ~pathloc expr in
    <:html<$keyword "functor"$ () -> $rhs$>>, rest
  | Functor (Some (arg_ident, arg_sig), expr) ->
    let rhs_sig, rest_sig = rhs_rest_of_sig ~pathloc arg_sig in
    let rhs, rest = rhs_rest_of_sig ~pathloc expr in
    let arg = name_of_ident (Identifier.any arg_ident) in (* TODO: more? *)
    <:html<$keyword "functor"$ ($str:arg$ : $rhs_sig$$rest_sig$) -> $rhs$>>,
    rest
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
    (<:html<$keyword "module"$ $name$ = $rhs$ $rest$>> :: acc) subs
  | (TypeEq (type_frag, type_eqn))::subs ->
    (* TODO: private *)
    let { TypeDecl.Equation.params; private_; manifest } = type_eqn in
    let name = link_fragment ~pathloc base (Fragment.any type_frag) in
    let params = of_type_params params in
    let manifest = match manifest with
      | Some m -> <:html< = $of_type_expr ~pathloc m$>>
      | None -> <:html<&>>
    in
    <:html<$keyword "type"$ $params$$name$$manifest$>>
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
    <:html<$keyword "type"$ $params$$name$ := $params$$type_path$>>
  | [] -> match acc with
    | [] -> <:html<&>>
    | [one] -> one
    | h::t -> fold_html <:html< $keyword "and"$ >> h t
)

and of_module_type ~pathloc { ModuleType.id; doc; expr } =
  let doc = maybe_div_doc ~pathloc doc in
  let sig_outro expr = (* TODO: fixme. use! *)
    let pathloc = { pathloc with
      unit_depth = pathloc.unit_depth + 1;
      internal_path = Identifier.module_type_signature id;
    } in
    rhs_rest_of_sig ~pathloc expr
  in
  let id = Identifier.any id in
  let name = name_of_ident id in
  let id = id_of_ident ~pathloc id in
  let rhs, rest = match expr with
    | None -> <:html<&>>, <:html<&>>
    | Some expr ->
      let rhs, rest = rhs_rest_of_sig ~pathloc expr in
      <:html<= $rhs$>>, rest
  in
  module_type_declaration ~id name rhs doc rest

and of_include ~pathloc module_type_expr =
  let rhs, rest = rhs_rest_of_sig ~pathloc module_type_expr in
  <:html<
    <div class="include">
    $keyword "include"$ $rhs$ $rest$
    </div>
  >> (* TODO: test *)

and of_signature_item ~pathloc = Signature.(function
  | Value val_ -> of_value ~pathloc val_
  | External ext -> of_external ~pathloc ext
  | Type type_ -> of_type ~pathloc type_
  | TypExt ext -> of_type_ext ~pathloc ext
  | Exception exn -> of_exception ~pathloc exn
  | Class class_ -> of_class ~pathloc class_
  | ClassType class_type -> of_class_type ~pathloc class_type
  | Module module_ -> of_module ~pathloc module_
  | ModuleType module_type -> of_module_type ~pathloc module_type
  | Include module_type_expr -> of_include ~pathloc module_type_expr
  | Comment comment -> of_comment ~pathloc comment
)

and of_signature ~pathloc signature =
  <:html<$list:List.map (of_signature_item ~pathloc) signature$>>

let of_top_module ~pathloc { Module.id; doc; type_ } =
  let doc = maybe_div_doc ~pathloc doc in
  let id = Identifier.any id in
  let link_ident_map = new link_ident_map ~pathloc () in
  let name = map_ident link_ident_map id in
  let rhs, rest = rhs_rest_of_decl ~pathloc type_ in
  let extra_classes = ["ocamlary-doc"] in
  let title_fn x = <:html<<h1 class="title">$x$</h1>&>> in
  module_declaration ~extra_classes ~title_fn ~id ~pathloc name rhs doc rest

let of_unit ~pathloc { Unit.id; doc; digest; imports; items } =
  (* TODO: more? *)
  of_top_module ~pathloc Module.({
    id; doc; type_ = ModuleType (ModuleType.Signature items);
  })
