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

let index_ns = "https://opam.ocaml.org/packages/codoc/xmlns/doc-index/0/#"

exception ParseError of string * Xmlm.pos option * Xmlm.pos * string

let doc_parser = DocOckXmlParse.build (fun input ->
  match Xmlm.input_tree
    ~el:(CodocDoc.root_of_xml DocOckXml.ns)
    ~data:CodocDoc.data_of_xml
    input
  with None -> failwith "can't find root" (* TODO: fixme *)
  | Some root -> root
)

let doc_printer output acc root =
  let rec fold acc = function
    | `Data s -> output acc (`Data s)
    | `El (tag, children) ->
      let acc = output acc (`El_start tag) in
      let acc = List.fold_left fold acc children in
      output acc `El_end
  in
  fold acc (List.hd (CodocDoc.xml_of_root DocOckXml.ns root))

let string_of_pos (line,col) =
  Printf.sprintf "line %d, col %d" line col

let rec must_end xml = match Xmlm.input xml with
  | `El_end -> ()
  | `Data _ -> must_end xml (* TODO: ? *)
  | _ -> (* TODO: fixme *)
    failwith (Printf.sprintf "expected end: %s" (string_of_pos (Xmlm.pos xml)))

let eat xml = ignore (Xmlm.input xml)

let just_data xml = match Xmlm.peek xml with
  | `Data data -> eat xml; data
  | _ -> (* TODO: fixme *) failwith "expected data"
