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

type path = string

type root =
| Cmti of path * string
| Xml of path * root
| Html of path * root

let rec name_of_root = function
  | Cmti (_, name) -> name
  | Xml (_, r) -> name_of_root r
  | Html (_, r) -> name_of_root r

let path_of_root = function
  | Cmti (path, _) | Xml (path, _) | Html (path, _) -> path

let rec xml_of_root = function
  | Cmti (cmti_path,name) ->
    <:xml<<cmti name=$str:name$ src=$str:cmti_path$ />&>>
  | Xml (xml_path,source) ->
    <:xml<<xml src=$str:xml_path$>$xml_of_root source$</xml>&>>
  | Html (html_path,source) ->
    <:xml<<html src=$str:html_path$>$xml_of_root source$</html>&>>

let filter_children = List.fold_left (fun l -> function
  | None -> l
  | Some v -> v::l
) []

let root_of_xml tag root_opt_list =
  match tag with
  | (("","cmti"),attrs) -> (* TODO: cmti can't have children *)
    Some List.(Cmti (assoc ("","src") attrs, assoc ("","name") attrs))
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
