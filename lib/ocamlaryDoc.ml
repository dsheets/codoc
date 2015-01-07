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

open DocOckPaths
open OcamlaryDocMaps

type path = string

type root =
| Cmti of path * string
(* TODO: use signature identifier when doc-ock-xml supports it *)
| Proj of (*root DocOckPaths.Identifier.signature*) string * root
| Xml of path * root
| Html of path * root

module Root = struct
  type t = root

  class virtual named = object (self : 'self)
    constraint 'self = (unit, t, string) #Identifier.any_fold

    method root_name () = function
    | Cmti (_, name) -> name
    | Proj (sig_, r) ->
      (*(self#name r)^"["^(map_ident self (Identifier.any sig_))^"]"*)
      (self#root_name () r)^"["^sig_^"]"
    | Xml (_, r) | Html (_, r) -> self#root_name () r
  end

  let rec to_source = function
    | Cmti _ as cmti -> cmti
    | Proj (_, r) | Xml (_, r) | Html (_, r) -> to_source r

  let rec to_path = function
    | Cmti (path, _) | Xml (path, _) | Html (path, _) -> path
    | Proj (_, root) -> to_path root
end

module Maps = OcamlaryDocMaps.Make(Root)
open Maps

type text = Root.t DocOckTypes.Documentation.text

type t =
| Para of text
| Block of text

let rec xml_of_root = Root.(function
  | Cmti (cmti_path,name) ->
    <:xml<<cmti name=$str:name$ src=$str:cmti_path$ />&>>
  | Proj (sig_,source) ->
    (* TODO: serialize with doc-ock-xml vocab *)
    (*let sig_ = Identifier.any sig_ in*)
    <:xml<<proj path=$str:sig_$>$xml_of_root source$</proj>&>>
  | Xml (xml_path,source) ->
    <:xml<<xml src=$str:xml_path$>$xml_of_root source$</xml>&>>
  | Html (html_path,source) ->
    <:xml<<html src=$str:html_path$>$xml_of_root source$</html>&>>
)

let filter_children = List.fold_left (fun l -> function
  | None -> l
  | Some v -> v::l
) []

(* TODO: handle exceptions (e.g. Not_found) *)
let root_of_xml tag root_opt_list =
  match tag with
  | (("","cmti"),attrs) -> (* TODO: cmti can't have children *)
    Some List.(Cmti (assoc ("","src") attrs, assoc ("","name") attrs))
  | (("","proj"),attrs) ->
    let root = match filter_children root_opt_list with
      | [] -> failwith "proj root must have a source" (* TODO: fixme *)
      | [root] -> root
      | _::_::_ -> failwith "proj root has too many children" (* TODO: fixme *)
    in
    (* TODO: deserialize with doc-ock-xml vocab *)
    Some (Proj (List.assoc ("","path") attrs, root))
  | (("","xml"),attrs) ->
    let root = match filter_children root_opt_list with
      | [] -> failwith "xml root must have a source" (* TODO: fixme *)
      | [root] -> root
      | _::_::_ -> failwith "xml root has too many children" (* TODO: fixme *)
    in
    Some (Xml (List.assoc ("","src") attrs, root))
  | (("","html"),attrs) ->
    let root = match filter_children root_opt_list with
      | [] -> failwith "html root must have a source" (* TODO: fixme *)
      | [root] -> root
      | _::_::_ -> failwith "html root has too many children" (* TODO: fixme *)
    in
    Some (Html (List.assoc ("","src") attrs, root))
  | _ -> None (* TODO: fixme *)

let data_of_xml _ = None    

let is_block = DocOckTypes.Documentation.(function
  | Raw _ | Code _ | Reference _
  | Style ((Bold | Italic | Emphasize | Superscript | Subscript), _)
  | Newline -> false

  | PreCode _ | Verbatim _ | List _ | Enum _ | Title _ | Special _
  | Style ((Center | Left | Right), _) -> true

  | Style (Custom _, _) -> false (* TODO: check *)
  | Target _ -> false (* TODO: check *)
)

let paragraphize txt =
  let rec collect paras acc els = DocOckTypes.Documentation.(match acc, els with
    | (Block [] | Para []), Newline::rest -> collect paras (Para []) rest
    | Para acc, Newline::rest ->
      collect (Para (List.rev acc)::paras) (Para []) rest
    | Para [], other::rest when is_block other ->
      collect paras (Block [other]) rest
    | Para acc, other::rest when is_block other ->
      collect (Para (List.rev acc)::paras) (Block [other]) rest
    | Para acc, other::rest -> collect paras (Para (other::acc)) rest
    | Block acc, other::rest when is_block other ->
      collect paras (Block (other::acc)) rest
    | Block acc, Newline::rest ->
      collect (Block (List.rev acc)::paras) (Para []) rest
    | Block acc, other::rest ->
      collect (Block (List.rev acc)::paras) (Para [other]) rest
    | Para acc, []  -> List.rev (Para (List.rev acc)::paras)
    | Block acc, [] -> List.rev (Block (List.rev acc)::paras)
  ) in
  collect [] (Para []) txt
