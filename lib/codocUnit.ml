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

open DocOck.Types

module Id = struct
  open CodocDoc
  open CodocDocMaps

  let type_class = "typ"
  let exn_class = "exn" (* exception *)
  let class_class = "cls"
  let class_type_class = "clst"
  let module_class = "mod"
  let module_type_class = "modt"

  (* TODO: use these for CSS class names? *)
  let ident_class = Identifier.(function
    | Root _ -> "root"
    | Module _ -> module_class
    | Argument _ -> "moda"
    | ModuleType _ -> module_type_class
    | Type _ -> type_class
    | CoreType _ -> type_class
    | Constructor _ -> "cons" (* const *)
    | Field _ -> "fld" (* recfield *)
    | Extension _ -> "ext"
    | Exception _ -> exn_class
    | CoreException _ -> exn_class
    | Value _ -> "val"
    | Class _ -> class_class
    | ClassType _ -> class_type_class
    | Method _ -> "meth"
    | InstanceVariable _ -> "var" (* attribute *)
    | Label _ -> "labl" (* section *)
  )

  let classify cls name = Printf.sprintf "%s.%s" name cls

  let name_of_argument i name = Printf.sprintf "%s.%d" name i
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
    id          : root DocOck.Paths.Identifier.any;
    fragment    : fragment;
    pkg_root    : string option;
    base        : string option;
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

  let loc ?pkg_root ?base scheme id =
    match fragment_of_ident id with
    | Some fragment ->
      let loc = {
        scheme;
        id;
        fragment;
        pkg_root;
        base;
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

  let uri_of_here loc frag =
    let full = frag.path in
    let path = match loc.base with None -> full | Some base -> base::full in
    Some (uri_of_diff loc 0 path frag.frag Uri.empty)

  let of_ident loc ident =
    let self = loc.fragment in
    match fragment_of_ident ident with
    | None -> None
    | Some frag ->
      let up, out = subtract frag self in
      match out.root with
      | None -> begin match loc.pkg_root with
        | Some "." -> uri_of_here loc frag
        | Some _ | None -> begin match out.path, out.frag with
          | [], [] when up = 0 && List.length self.path > 0 ->
            (* we have a parent! *)
            Some (uri_of_diff loc 1 [] List.[hd (rev self.path)] Uri.empty)
          | path, frag ->
            Some (uri_of_diff loc up path frag Uri.empty)
        end
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

  let ascent_of_ident id =
    match fragment_of_ident id with
    | None -> ""
    | Some fragment -> CodocUtil.ascent_of_depth "" (List.length fragment.path)
end
