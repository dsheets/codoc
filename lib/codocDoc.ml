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

open DocOck.Paths
open CodocDocMaps

type path = string

type cm_root = {
  unit_path : CodocExtraction.file;
  unit_name : string;
  unit_digest : Digest.t;
}

(* TODO: time? *)
type resolution = {
  resolution_root : string;
}

type root =
| Cm of cm_root
| Resolved of resolution * root
(* TODO: use signature identifier when doc-ock-xml supports it *)
| Proj of (*root DocOck.Paths.Identifier.signature*) string * root
| Xml of path * root

module Root = struct
  type t = root

  class virtual named = object (self : 'self)
    constraint 'self = (unit, t, string) #Identifier.any_fold

    method root_name () = function
    | Cm { unit_name } -> unit_name
    | Resolved (_, r) -> self#root_name () r
    | Proj (sig_, r) ->
      (*(self#name r)^"["^(map_ident self (Identifier.any sig_))^"]"*)
      (self#root_name () r)^"["^sig_^"]"
    | Xml (_, r) -> self#root_name () r
  end

  let rec to_source = function
    | Cm _ as cm -> cm
    | Resolved (_, r) | Proj (_, r) | Xml (_, r) -> to_source r

  let rec to_path = function
    | Cm { unit_path } -> CodocExtraction.path unit_path
    | Xml (path, _) -> path
    | Resolved (_, root) | Proj (_, root) -> to_path root

  let rec to_digest = function
    | Cm { unit_digest } -> unit_digest
    | Xml (_, root) | Resolved (_, root) | Proj (_, root) ->
      to_digest root

  let rec to_name = function
    | Cm { unit_name } -> unit_name
    | Xml (_, root) | Resolved (_, root) | Proj (_, root) ->
      to_name root

  let equal root root' = (to_source root) = (to_source root')
  let hash root = Hashtbl.hash (to_source root)
end

module Maps = CodocDocMaps.Make(Root)
open Maps

type text = Root.t DocOck.Types.Documentation.text

type t =
| Para of text
| Block of text

let rec xml_of_root xmlns = Root.(function
  | Cm { unit_path; unit_name = name; unit_digest = digest } ->
    let digest = Digest.to_hex digest in
    let attrs = [
      ("","name"),name;
      ("","src"),CodocExtraction.path unit_path;
      ("","digest"),digest;
    ] in
    [`El ((("","cm"),attrs), [])]
  | Resolved ({ resolution_root = root }, source) ->
    let attrs = [
      ("","root"),root;
    ] in
    [`El ((("","resolved"),attrs), xml_of_root xmlns source)]
  | Proj (sig_,source) ->
    (* TODO: serialize with doc-ock-xml vocab *)
    (*let sig_ = Identifier.any sig_ in*)
    let attrs = [
      ("","path"),sig_;
    ] in
    [`El ((("","proj"),attrs), xml_of_root xmlns source)]
  | Xml (xml_path,source) ->
    let attrs = [
      ("","src"),xml_path;
    ] in
    [`El ((("","xml"),attrs), xml_of_root xmlns source)]
)

let filter_children = List.fold_left (fun l -> function
  | None -> l
  | Some v -> v::l
) []

(* TODO: handle exceptions (e.g. Not_found) *)
let root_of_xml xmlns tag root_opt_list =
  match tag with
  | ((ns,"cm"),attrs) when ns = xmlns -> (* TODO: cm can't have children *)
    let src = List.assoc ("","src") attrs in
    let unit_path = match CodocExtraction.file src with
      | None ->
        failwith "unit extraction path is of unknown type" (* TODO: fixme *)
      | Some p -> p
    in
    let unit_name = List.assoc ("","name") attrs in
    let unit_digest = List.assoc ("","digest") attrs in
    let unit_digest = Digest.from_hex unit_digest in
    Some (Cm { unit_path; unit_name; unit_digest })
  | ((ns,"resolved"),attrs) when ns = xmlns ->
    let root = match filter_children root_opt_list with
      | [] -> failwith "resolved root must have a source" (* TODO: fixme *)
      | [root] -> root
      | _::_::_ -> failwith "resolved root has too many children"(* TODO: fixme *)
    in
    let resolution_root = List.assoc ("","root") attrs in
    Some (Resolved ({ resolution_root }, root))
  | ((ns,"proj"),attrs) when ns = xmlns ->
    let root = match filter_children root_opt_list with
      | [] -> failwith "proj root must have a source" (* TODO: fixme *)
      | [root] -> root
      | _::_::_ -> failwith "proj root has too many children" (* TODO: fixme *)
    in
    (* TODO: deserialize with doc-ock-xml vocab *)
    Some (Proj (List.assoc ("","path") attrs, root))
  | ((ns,"xml"),attrs) when ns = xmlns ->
    let root = match filter_children root_opt_list with
      | [] -> failwith "xml root must have a source" (* TODO: fixme *)
      | [root] -> root
      | _::_::_ -> failwith "xml root has too many children" (* TODO: fixme *)
    in
    Some (Xml (List.assoc ("","src") attrs, root))
  | _ -> None (* TODO: fixme *)

let data_of_xml _ = None    

let is_block = DocOck.Types.Documentation.(function
  | Raw _ | Code _ | Reference _
  | Style ((Bold | Italic | Emphasize | Superscript | Subscript), _)
  | Newline -> false

  | PreCode _ | Verbatim _ | List _ | Enum _ | Title _ | Special _
  | Style ((Center | Left | Right), _) -> true

  | Style (Custom _, _) -> false (* TODO: check *)
  | Target _ -> false (* TODO: check *)
)

let paragraphize txt =
  let rec collect paras acc els = DocOck.Types.Documentation.(match acc, els with
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

let first_sentence_of_string s =
  let len = String.length s in
  let rec f i = function
    | '.' -> let i = i + 1 in if i < len then g i s.[i] else None
    | _   -> let i = i + 1 in if i < len then f i s.[i] else None
  and g i = function
    | ' ' -> Some (String.sub s 0 i)
    | c -> f i c
  in
  if 0 < len then f 0 s.[0] else None

let rec first_sentence_of_text acc els = DocOck.Types.Documentation.(
  match els with
  | [] -> false, List.rev acc
  | (Raw s as el)::rest -> begin match first_sentence_of_string s with
    | None -> first_sentence_of_text (el::acc) rest
    | Some s -> true, List.rev ((Raw s)::acc)
  end
  | (Reference (Link _ as link, Some t) as el)::rest ->
    let found, t = first_sentence_of_text [] t in
    if found
    then found, List.rev (Reference (link, Some t)::acc)
    else first_sentence_of_text (el::acc) rest
  | (Style (style, t) as el)::rest ->
    let found, t = first_sentence_of_text [] t in
    if found
    then found, List.rev (Style (style, t)::acc)
    else first_sentence_of_text (el::acc) rest
  | (Newline | Title _ | Special _ | List _
    | Enum _ | PreCode _ | Verbatim _)::_ ->
    true, List.rev acc
  | (Code _ | Target _ | Reference (_, _) as el)::rest ->
    first_sentence_of_text (el::acc) rest
)
let first_sentence_of_text text = snd (first_sentence_of_text [] text)
