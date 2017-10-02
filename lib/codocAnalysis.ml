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

type doc_error =
  | Empty_section of string
  | Empty_tag of string
  | Unlinked_ref of string
  | Unlinked_path of string
  | Unlinked_frag of string
  | Empty_code
  | Bad_stop
  | Doc_error of string

let xmlns = CodocXml.index_ns

let string_of_error = function
  | Empty_section label -> "empty section '"^label^"'"
  | Empty_tag tag -> "empty tag '"^tag^"'"
  | Unlinked_ref name -> "unresolved reference '"^name^"'"
  | Unlinked_path name -> "unresolved path '"^name^"'"
  | Unlinked_frag name -> "unresolved fragment '"^name^"'"
  | Empty_code -> "empty code section ([])"
  | Bad_stop -> "ineffective stop comment in item documentation"
  | Doc_error message -> message

let xml_of_error = function
  | Empty_section label ->
    [`El ((("","empty-section"),[("","label"),label]),[])]
  | Empty_tag tag ->
    [`El ((("","empty-tag"),[("","tag"),tag]),[])]
  | Unlinked_ref name ->
    [`El ((("","unlinked-ref"),[("","name"),name]),[])]
  | Unlinked_path name ->
    [`El ((("","unlinked-path"),[("","name"),name]),[])]
  | Unlinked_frag name ->
    [`El ((("","unlinked-frag"),[("","name"),name]),[])]
  | Empty_code -> [`El ((("","empty-code"),[]),[])]
  | Bad_stop -> [`El ((("","bad-stop"),[]),[])]
  | Doc_error message ->
    [`El ((("","doc-error"),[("","message"),message]),[])]

let rec error_of_xml xml = match Xmlm.peek xml with
  | `El_start ((ns,"empty-section"),[("","label"),label]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Empty_section label
  | `El_start ((ns,"empty-tag"),[("","tag"),tag]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Empty_tag tag
  | `El_start ((ns,"unlinked-ref"),[("","name"),name]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Unlinked_ref name
  | `El_start ((ns,"unlinked-path"),[("","name"),name]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Unlinked_path name
  | `El_start ((ns,"unlinked-frag"),[("","name"),name]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Unlinked_frag name
  | `El_start ((ns,"empty-code"),[]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Empty_code
  | `El_start ((ns,"bad-stop"),[]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Bad_stop
  | `El_start ((ns,"doc-error"),[("","message"),message]) when ns = xmlns ->
    CodocXml.eat xml;
    CodocXml.must_end xml;
    Doc_error message
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
  | `El_end -> (* TODO: fixme *) failwith "unexpected end"
  | `Data _ | `Dtd _ -> CodocXml.eat xml; error_of_xml xml

open DocOck.Paths

module MyRef = struct
  open Reference

  module Resolved = struct
    open CodocDocMaps.Reference_resolved
    class to_string_map = object (self : 'self)
      constraint 'self = (unit, CodocDoc.root, string, kind) #any_fold
      inherit [unit, CodocDoc.root, string] any_parent_fold

      method parent () _ref parent name =
        (fold_any self () (Reference.Resolved.any parent)) ^ "." ^ name

      method identifier () = CodocDoc.Maps.string_of_ident
    end

    let to_string_map = new to_string_map

    let to_string = fold_any to_string_map ()
  end

  let rec to_string = function
    | Resolved r -> Resolved.to_string r
    | Root name -> name
    | Dot (parent, name) -> to_string (Reference.any parent) ^ "." ^ name
end

module MyPath = struct
  open Path

  let rec to_string rf = function
    | Resolved r -> rf r
    | Root name -> name
    | Dot (parent, name) -> to_string rf (any parent) ^ "." ^ name
    | Apply (module_,module') ->
      to_string rf (any module_) ^ "(" ^
      to_string rf (any module') ^ ")"

  module Resolved = struct
    open CodocDocMaps.Path_resolved
    class to_string_map = object (self : 'self)
      constraint 'self = (unit, CodocDoc.root, string, kind) #any_fold
      inherit [unit, CodocDoc.root, string] any_parent_fold

      method parent () _ref parent name =
        (fold_any self () (Path.Resolved.any parent)) ^ "." ^ name

      method identifier () ident =
        CodocDoc.Maps.string_of_ident (Identifier.any ident)

      method subst () _rsub li = fold_any self () (any li)
      method subst_alias () _rsub li = fold_any self () (any li)
      method apply () f a =
        let rf = fold_any self () in
        let a = to_string rf (Path.any a) in
        fold_any self () (any f) ^ "(" ^ a ^ ")"
    end

    let to_string_map = new to_string_map

    let to_string = fold_any to_string_map ()
  end

  let to_string = to_string Resolved.to_string
end

module MyFrag = struct
  open Fragment

  module Resolved = struct
    open CodocDocMaps.Fragment_resolved
    class to_string_map = object (self : 'self)
      constraint 'self =
        (unit, CodocDoc.root, string, kind, [`Branch]) #any_fold
      inherit [unit, CodocDoc.root, string, kind, [`Branch]] any_parent_fold

      method parent () _frag p name =
        let rec render : 'a Resolved.signature -> string = function
          | Root -> ""
          | Subst (_,li) -> render li
          | SubstAlias (_,li) -> render li
          | Module _ as parent -> (fold_any self () parent) ^ "." ^ name
        in
        render p
      method subst () _rsub li = fold_any self () li
      method subst_alias () _rsub li = fold_any self () li
    end

    let to_string_map = new to_string_map

    let to_string = fold_any to_string_map ()
  end

  let rec to_string = function
    | Resolved r -> Resolved.to_string r
    | Dot ((Dot _ | Resolved (Fragment.Resolved.Module _)) as parent, name) ->
      to_string parent ^ "." ^ name
    | signature -> match split signature with
      | name, None -> name
      | name, Some parent -> to_string parent ^ "." ^ name

end

class ['a] index = object (self)
  val mutable issues = []
  val mutable top = true
  val mutable current_tag = None
  val mutable current_label = None

  inherit ['a] DocOckMaps.types as super
  inherit ['a] DocOckMaps.reference
  inherit ['a] DocOckMaps.path
  inherit ['a] DocOckMaps.fragment

  method root x = x
  method identifier_value x = x
  method identifier_type x = x
  method identifier_module_type x = x
  method identifier_signature x = x
  method identifier_module x = x
  method identifier_method x = x
  method identifier_label x = x
  method identifier_instance_variable x = x
  method identifier_field x = x
  method identifier_extension x = x
  method identifier_exception x = x
  method identifier_constructor x = x
  method identifier_class_type x = x
  method identifier_class x = x
  method identifier x = x

  (* Catch unresolved names *)

  method reference : type k . ('a, k) Reference.t -> ('a, k) Reference.t =
    fun r ->
      let open Reference in
      match r with
      | Resolved _ -> r
      | Root name -> self#unlinked_ref name; r
      | Dot (_,_) -> self#unlinked_ref (MyRef.to_string (any r)); r

  method path : type k . ('a, k) Path.t -> ('a, k) Path.t =
    fun p ->
      let open Path in
      match p with
      | Resolved _ -> p
      | Root name -> self#unlinked_path name; p
      | Dot (_,_) -> self#unlinked_path (MyPath.to_string (any p)); p
      | Apply (_,_) -> self#unlinked_path (MyPath.to_string (any p)); p

  method fragment : type k s. ('a, k, s) Fragment.raw ->
                                     ('a, k, s) Fragment.raw =
    fun p ->
      let open Fragment in
      match p with
      | Resolved _ -> p
      | Dot (_,_) as p -> self#unlinked_frag (MyFrag.to_string p); p

  (* Don't go into subcomponents *)

  method signature s =
    if top
    then (top <- false; super#signature s)
    else s

  method module_ m =
    if top
    then super#module_ m
    else (ignore (self#module_decl m.DocOck.Types.Module.type_); m)

  method module_type m =
    if top
    then super#module_type m
    else match m.DocOck.Types.ModuleType.expr with
      | None -> m
      | Some expr -> ignore (self#module_type_expr expr); m

  method class_ c =
    if top
    then (top <- false; super#class_ c)
    else c

  method class_type c =
    if top
    then (top <- false; super#class_type c)
    else c

  method unit u =
    if top
    then super#unit u
    else (ignore (self#unit_content u.DocOck.Types.Unit.content); u)

  (* Check for missing tag content *)

  method documentation_see_url see = match see with
    | "" -> self#empty_tag "see url"; see
    | _ -> see
  method documentation_see_file see = match see with
    | "" -> self#empty_tag "see file"; see
    | _ -> see
  method documentation_see_doc see = match see with
    | "" -> self#empty_tag "see doc"; see
    | _ -> see

  method documentation_text txt = match txt with
    | [] | [DocOck.Types.Documentation.Raw ""] -> begin match current_tag with
      | Some tag -> self#empty_tag tag; txt
      | None -> match current_label with
        | Some label -> self#empty_label label; txt
        | None -> txt
    end
    | [DocOck.Types.Documentation.Raw "/*"] -> self#bad_stop; txt
    | x -> super#documentation_text txt

  method with_tag tag f =
    current_tag <- Some tag;
    ignore (f ());
    current_tag <- None

  method documentation_tag tag =
    let open DocOck.Types.Documentation in
    match tag with
    | Author "" -> self#empty_tag "author"; tag
    | Author _ -> tag
    | Version "" -> self#empty_tag "version"; tag (* TODO: currently early *)
    | Version _ -> tag
    | See(see, _) -> ignore (self#documentation_see see); tag
    | Since "" -> self#empty_tag "since"; tag (* TODO: never fires? *)
    | Since _ -> tag
    | Before("",_) -> self#empty_tag "before"; tag (* TODO: currently early *)
    | Before(_, txt) ->
      self#with_tag "before" (fun () -> self#documentation_text txt); tag
    | Deprecated _ -> tag (* reasonable to be empty *)
    | Param("", txt) -> self#empty_tag "param"; tag (* TODO: currently early *)
    | Param(name, txt) ->
      self#with_tag ("param "^name) (fun () -> self#documentation_text txt);
      tag
    | Raise("", _) -> self#empty_tag "raise"; tag (* TODO: currently early *)
    | Raise(_, _) -> tag
    | Return txt ->
      self#with_tag "return" (fun () -> self#documentation_text txt); tag
    | Inline -> tag
    | Tag("", _) -> self#empty_tag "custom"; tag
    | Tag(_, _) -> tag

  (* Check for early documentation errors *)

  method documentation_error err =
    let open DocOck.Types.Documentation.Error in
    let string_of_pos position = Position.(
      string_of_int position.line ^":"^ string_of_int position.column
    ) in
    let location = Location.(match err.location with
      | None -> ""
      | Some { filename; start; finish } ->
        filename^":"^string_of_pos start^"-"^string_of_pos finish^" "
    ) in
    let offset = Offset.(
      string_of_pos err.offset.start ^"-"^ string_of_pos err.offset.finish
    ) in
    self#doc_error (location^"@ "^offset^" : "^err.message);
    err

  (* Check for empty sections *)

  method documentation_text_element elem =
    let open DocOck.Types.Documentation in
    match elem with
    | Code "" -> self#empty_code; elem
    | Raw _
    | Code _
    | PreCode _
    | Verbatim _
    | Style(_, _)
    | List _
    | Title(_, None, _)
    | Reference(_, _)
    | Target(_, _)
    | Special _
    | Enum _ -> super#documentation_text_element elem
    | Newline -> elem
    | Title(_, Some label, text) ->
      let label = DocOck.Paths.Identifier.any label in
      current_label <- Some (CodocDoc.Maps.string_of_ident label);
      ignore (self#documentation_text text);
      current_label <- None;
      super#documentation_text_element elem

  (* Issue constructors and accessors *)

  method empty_tag s = issues <- (Empty_tag s)::issues
  method empty_label s = issues <- (Empty_section s)::issues
  method empty_code = issues <- Empty_code::issues
  method unlinked_ref s = issues <- (Unlinked_ref s)::issues
  method unlinked_path s = issues <- (Unlinked_path s)::issues
  method unlinked_frag s = issues <- (Unlinked_frag s)::issues
  method bad_stop = issues <- Bad_stop::issues
  method doc_error s = issues <- (Doc_error s)::issues
  method issues = issues
end

let of_class x =
  let obj = new index in
  ignore (obj#class_ x);
  obj#issues

let of_class_type x =
  let obj = new index in
  ignore (obj#class_type x);
  obj#issues

let of_module x =
  let obj = new index in
  ignore (obj#module_ x);
  obj#issues

let of_module_type x =
  let obj = new index in
  ignore (obj#module_type x);
  obj#issues

let of_argument (id, expr) =
  let obj = new index in
  ignore (obj#identifier_module id);
  ignore (obj#module_type_expr expr);
  obj#issues

let of_unit x =
  let obj = new index in
  ignore (obj#unit x);
  obj#issues

