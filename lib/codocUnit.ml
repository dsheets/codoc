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

open DocOckTypes

module Id = struct
  open CodocDoc
  open CodocDocMaps

  let type_class = "typ"
  let exn_class = "exn" (* exception *)
  let class_class = "cls"
  let classtype_class = "clst"
  let module_class = "mod"
  let moduletype_class = "modt"

  (* TODO: use these for CSS class names? *)
  let ident_class = Identifier.(function
    | Root _ -> "root"
    | Module _ -> module_class
    | Argument _ -> "moda"
    | ModuleType _ -> moduletype_class
    | Type _ -> type_class
    | CoreType _ -> type_class
    | Constructor _ -> "cons" (* const *)
    | Field _ -> "fld" (* recfield *)
    | Extension _ -> "ext"
    | Exception _ -> exn_class
    | CoreException _ -> exn_class
    | Value _ -> "val"
    | Class _ -> class_class
    | ClassType _ -> classtype_class
    | Method _ -> "meth"
    | InstanceVariable _ -> "var" (* attribute *)
    | Label _ -> "labl" (* section *)
  )

  let classify cls name = Printf.sprintf "%s.%s" name cls

  let name_of_argument i name = Printf.sprintf "%s.%d" name i
end

module Substruct = struct

  type ('a,'b) map = {
    map_class : 'a -> CodocDoc.root Class.t -> 'b;
    map_classtype : 'a -> CodocDoc.root ClassType.t -> 'b;
    map_module : 'a -> CodocDoc.root Module.t -> 'b;
    map_moduletype : 'a -> CodocDoc.root ModuleType.t -> 'b;
  }

  type 'a collect = ('a,'a) map

  type 'a t =
    | Class of CodocDoc.root Class.t * 'a
    | ClassType of CodocDoc.root ClassType.t * 'a
    | Module of CodocDoc.root Module.t * 'a t list * 'a
    | ModuleType of CodocDoc.root ModuleType.t * 'a t list * 'a

  type 'a name =
    | ClassName of string * 'a
    | ClassTypeName of string * 'a
    | ModuleName of string * 'a name list * 'a
    | ModuleTypeName of string * 'a name list * 'a

  type id =
    | Classy of CodocDoc.root DocOckPaths.Identifier.class_signature
    | Moduley of CodocDoc.root DocOckPaths.Identifier.signature

  let classtype_expr_is_signature = ClassType.(function
    | Constr _ -> false
    | Signature _ -> true
  )

  let classtype_is_signature c = classtype_expr_is_signature c.ClassType.expr

  let rec class_decl_has_signature = Class.(function
    | Arrow (_,_,c) -> class_decl_has_signature c
    | ClassType c -> classtype_expr_is_signature c
  )

  let class_has_signature c = class_decl_has_signature c.Class.type_

  let collect_sig_structs f acc = Signature.(function
    | Value _ | External _ | Type _ | TypExt _ | Exception _
    | Include _ | Comment _ -> acc
    | Class c -> if class_has_signature c then f.map_class acc c else acc
    | ClassType c ->
      if classtype_is_signature c then f.map_classtype acc c else acc
    | Module m -> f.map_module acc m
    | ModuleType m -> f.map_moduletype acc m
  )

  let rec collect_eqn_structs f acc = Module.(function
    | Alias _ -> None
    | ModuleType type_ -> collect_module_type_expr_structs f acc type_
  )
  and collect_module_type_expr_structs f acc = ModuleType.(function
    | Path _ -> None
    | Signature s -> Some (List.fold_left (collect_sig_structs f) acc s)
    | Functor (_, _) -> Some acc
    | With (expr, _) -> collect_module_type_expr_structs f acc expr
    | TypeOf eqn -> collect_eqn_structs f acc eqn
  )

  let collect_module_structs f acc { Module.type_ } =
    collect_eqn_structs f acc type_

  let collect_module_type_structs f acc = ModuleType.(function
    | { expr = Some expr } -> collect_module_type_expr_structs f acc expr
    | { expr = None } -> None
  )

  let rec substruct_collect f = {
    map_class = (fun l c -> (Class (c, f.map_class () c))::l);
    map_classtype = (fun l c -> (ClassType (c, f.map_classtype () c))::l);
    map_module = (fun l m ->
      match collect_module_structs (substruct_collect f) [] m with
      | Some subs -> (Module (m,subs,f.map_module () m))::l
      | None -> l
    );
    map_moduletype = (fun l m ->
      match collect_module_type_structs (substruct_collect f) [] m with
      | Some subs -> (ModuleType (m,subs,f.map_moduletype () m))::l
      | None -> l
    );
  }

  let map_of_unit_signature f subs { Unit.doc; id; } items =
    let modu = Module.(
      { id; doc; type_ = ModuleType (ModuleType.Signature items) }
    ) in
    Module (modu, subs, f.map_module () modu)

  let root_of_unit_signature = map_of_unit_signature {
    map_module = (fun () _ -> ());
    map_moduletype = (fun () _ -> ());
    map_class = (fun () _ -> ());
    map_classtype = (fun () _ -> ());
  } []

  let map_of_unit f = function
    | { Unit.content = Unit.Module signature } as unit ->
      let subs =
        List.fold_left (collect_sig_structs (substruct_collect f)) [] signature
      in
      Some (map_of_unit_signature f subs unit signature)
    | { Unit.content = Unit.Pack _ } -> (* TODO: support packs *)
      None

  let rec map f = function
    | Class (c,a) -> Class (c, f.map_class a c)
    | ClassType (c,a) -> ClassType (c, f.map_classtype a c)
    | Module (m,l,a) -> Module (m, List.map (map f) l, f.map_module a m)
    | ModuleType (m,l,a) ->
      ModuleType (m, List.map (map f) l, f.map_moduletype a m)

  let rec fold f acc = function
    | Class (c,a) -> f.map_class (acc,a) c
    | ClassType (c,a) -> f.map_classtype (acc,a) c
    | Module (m,l,a) -> f.map_module (List.fold_left (fold f) acc l,a) m
    | ModuleType (m,l,a) ->
      f.map_moduletype (List.fold_left (fold f) acc l,a) m

  let compose a b = {
    map_class = (fun x c -> b.map_class (a.map_class x c) c);
    map_classtype = (fun x c -> b.map_classtype (a.map_classtype x c) c);
    map_module = (fun x m -> b.map_module (a.map_module x m) m);
    map_moduletype = (fun x m -> b.map_moduletype (a.map_moduletype x m) m);
  }

  let homo_map f =
    let f x _ = f x in {
      map_class = f;
      map_classtype = f;
      map_module = f;
      map_moduletype = f;
    }

  let ident_map = {
    map_class = DocOckPaths.Identifier.(fun _ c ->
      parent_of_class_signature
        (class_signature_of_class c.DocOckTypes.Class.id)
    );
    map_classtype = DocOckPaths.Identifier.(fun _ c ->
      parent_of_class_signature
        (class_signature_of_class_type c.DocOckTypes.ClassType.id)
    );
    map_module = DocOckPaths.Identifier.(fun _ m ->
      parent_of_signature (signature_of_module m.DocOckTypes.Module.id)
    );
    map_moduletype = DocOckPaths.Identifier.(fun _ m ->
      parent_of_signature (signature_of_module_type m.DocOckTypes.ModuleType.id)
    );
  }

  let list_option_fold () = homo_map (fun (list,next) -> match next with
    | None -> list
    | Some next -> next::list
  )

  let id = function
    | Module ({ DocOckTypes.Module.id }, _, _) ->
      Moduley (DocOckPaths.Identifier.signature_of_module id)
    | ModuleType ({ DocOckTypes.ModuleType.id }, _, _) ->
      Moduley (DocOckPaths.Identifier.signature_of_module_type id)
    | Class ({ DocOckTypes.Class.id }, _) ->
      Classy (DocOckPaths.Identifier.class_signature_of_class id)
    | ClassType ({ DocOckTypes.ClassType.id }, _) ->
      Classy (DocOckPaths.Identifier.class_signature_of_class_type id)

  let class_name c =
    DocOckPaths.Identifier.name c.DocOckTypes.Class.id
  let classtype_name c =
    DocOckPaths.Identifier.name c.DocOckTypes.ClassType.id
  let module_name m =
    DocOckPaths.Identifier.name m.DocOckTypes.Module.id
  let moduletype_name m =
    DocOckPaths.Identifier.name m.DocOckTypes.ModuleType.id

  let rec to_name = function
    | Class (c,a) -> ClassName (class_name c, a)
    | ClassType (c,a) -> ClassTypeName (classtype_name c, a)
    | Module (m,l,a) ->
      ModuleName (module_name m, List.map to_name l, a)
    | ModuleType (m,l,a) ->
      ModuleTypeName (moduletype_name m, List.map to_name l, a)

  let string_of_name = function
    | ClassName (name,_) -> name
    | ClassTypeName (name,_) -> name
    | ModuleName (name,_,_) -> name
    | ModuleTypeName (name,_,_) -> name
end

module Href = struct
  open CodocDoc
  open CodocDocMaps

  type fragment = {
    root : root option;
    path : string list;
    frag : string list;
  }

  type loc = {
    scheme      : string;
    id          : root DocOckPaths.Identifier.parent;
    fragment    : fragment;
    pkg_root    : string option;
  }

  let empty = { root = None; path = []; frag = [] }

  class fragment_of_ident_map :
    [fragment option, root, fragment option] Identifier.any_fold =
    object (self)
      inherit [fragment option, root, fragment option]
          Identifier.any_container_fragment_fold

      method root fragment root _name =
        Some { (match fragment with
          | None -> empty
          | Some frag -> frag
        ) with root = Some root }
      method core_type _fragment _name = None (* TODO: fixme *)
      method core_exception _fragment name = None (* TODO: fixme *)
      method argument fragment parent i name =
        self#signature fragment
          (Identifier.Argument (parent, i, name))
          (Identifier.parent_of_signature parent)
          (Id.name_of_argument i name)
      method private signature f signature parent name =
        let f = match f with
          | None -> empty
          | Some f -> f
        in
        let ident = Identifier.any signature in
        let cls = Id.ident_class ident in
        let f = Some { f with path = Id.classify cls name :: f.path } in
        Identifier.(fold_any self f (any parent))
      method private class_signature f class_signature parent name =
        let f = match f with
          | None -> empty
          | Some f -> f
        in
        let ident = Identifier.any class_signature in
        let cls = Id.ident_class ident in
        let f = Some { f with path = Id.classify cls name :: f.path } in
        Identifier.(fold_any self f (any parent))
      method private fragment f frag parent name =
        let f = match f with
          | None -> empty
          | Some f -> f
        in
        let ident = Identifier.any frag in
        let cls = Id.ident_class ident in
        let f = Some { f with frag = Id.classify cls name :: f.frag } in
        Identifier.(fold_any self f (any parent))
    end

  let fragment_of_ident_map = new fragment_of_ident_map

  let fragment_of_ident = Identifier.fold_any fragment_of_ident_map None

  let html_name_of path =
    (try
       let last_dot = String.rindex path '.' in
       String.sub path 0 last_dot
     with Not_found -> path
    )^ ".html"

  let uri_of_path ~scheme path =
    Uri.of_string begin
      if scheme <> "file" && Filename.check_suffix path "/index.html"
      then Filename.chop_suffix path "index.html"
      else path
    end

  let normal_uri_for_scheme scheme =
    if scheme <> "file"
    then fun uri -> uri
    else fun uri -> Uri.(resolve "" uri (of_string "index.html"))

  let normal_uri { scheme } = normal_uri_for_scheme scheme

  let index { scheme } = function
    | Xml (path, _) -> Some (uri_of_path ~scheme (html_name_of path))
    | _ -> None

  let print_fragment loc = function
    | { root = None; path; frag } ->
      prerr_endline "no root";
      prerr_endline (String.concat " :: " path);
      prerr_endline (String.concat " :: " frag)
    | { root = Some root; path; frag } ->
      begin match index loc root with
        | None -> prerr_endline "index failed"
        | Some uri -> prerr_endline (Uri.to_string uri)
      end;
      prerr_endline (String.concat " :: " path);
      prerr_endline (String.concat " :: " frag)

  let loc ?pkg_root scheme substruct =
    let id = Substruct.(match id substruct with
      | Classy c -> DocOckPaths.Identifier.parent_of_class_signature c
      | Moduley m -> DocOckPaths.Identifier.parent_of_signature m
    ) in
    match fragment_of_ident (Identifier.any id) with
    | Some fragment ->
      let loc = {
        scheme;
        id;
        fragment;
        pkg_root;
      } in
      (*print_fragment loc fragment;*)
      Some loc
    | None -> None

  let up loc = match loc.fragment.path with
    | _::_ -> Some (normal_uri loc (Uri.of_string "../"))
    | [] -> match loc.pkg_root with
      | Some pkg_root ->
        Some (normal_uri loc (Uri.of_string pkg_root))
      | None -> None

  let rec subtract_list target base = match target, base with
    | x::xs, y::ys when x = y -> subtract_list xs ys
    | _, _ -> List.length base, target

  let subtract target base =
    if target.root = base.root
    then let up, path = subtract_list target.path base.path in
      up, { root = None; path; frag = target.frag }
    else
      List.length base.path, target

  let concat a b = a^"/"^b

  let string_of_frag_pieces = List.fold_left concat ""

  let uri_of_diff loc up path frag base =
    let base = CodocUtil.ascent_of_depth (Uri.to_string base) up in
    let page = match path with
      | [] -> Uri.empty
      | path ->
        let path = List.fold_left (fun a b -> a^b^"/") "" path in
        normal_uri loc (Uri.with_path Uri.empty path)
    in
    let page = Uri.(resolve "" (normal_uri loc (of_string base)) page) in
    let uri_frag = match frag with
      | [] -> None
      | xs -> Some (string_of_frag_pieces xs)
    in
    Uri.with_fragment page uri_frag

  let of_ident loc ident =
    let self = loc.fragment in
    match fragment_of_ident ident with
    | None -> None
    | Some frag ->
      let up, out = subtract frag self in
      match out.root with
      | None -> begin match out.path, out.frag with
        | [], [] when List.length self.path > 0 -> (* we have a parent! *)
          Some (uri_of_diff loc 1 [] List.[hd (rev self.path)] Uri.empty)
        | path, frag ->
          Some (uri_of_diff loc up path frag Uri.empty)
      end
      | Some root -> match index loc root with
        | None -> None (* TODO: log? *)
        | Some uri ->
          Some (uri_of_diff loc up out.path out.frag uri)

  let id_of_ident loc ident =
    let self = loc.fragment in
    match fragment_of_ident ident with
    | None -> None
    | Some frag -> match subtract frag self with
      | 0, { root = None; path; frag } ->
        Some (string_of_frag_pieces path ^ string_of_frag_pieces frag)
      | _, _ -> None

  let of_name name = Substruct.(
    let seg = match name with
      | ClassName (name,_) -> Id.(classify class_class name)
      | ClassTypeName (name,_) -> Id.(classify classtype_class name)
      | ModuleName (name, _, _) -> Id.(classify module_class name)
      | ModuleTypeName (name, _, _) -> Id.(classify moduletype_class name)
    in
    Uri.of_string (seg^"/")
  )

  let ascent_of_ident id =
    match fragment_of_ident id with
    | None -> ""
    | Some fragment -> CodocUtil.ascent_of_depth "" (List.length fragment.path)
end
