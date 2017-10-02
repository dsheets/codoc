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

type typ = Cmti | Cmi | Cmt

let classify_path path =
  let open Filename in
  if check_suffix path ".cmti" then
    Some (Cmti, chop_suffix path ".cmti")
  else if check_suffix path ".cmi" then
    Some (Cmi, chop_suffix path ".cmi")
  else if check_suffix path ".cmt" then
    Some (Cmt, chop_suffix path ".cmt")
  else
    None

let is_extractable path =
  match classify_path path with
  | None -> false
  | Some _ -> true

type src = {
 rel : string;
 cmti: bool;
 cmt: bool;
 cmi: bool;
}

module StringMap = Map.Make(String)

type t = {
  root : string;
  srcs  : src StringMap.t;
}

let empty ~root =
  { root; srcs = StringMap.empty; }

let add t path =
  match classify_path path with
  | None -> t
  | Some (typ, path) ->
    let source =
      try
        StringMap.find path t.srcs
      with Not_found ->
        { rel = path; cmti = false; cmt = false; cmi = false; }
    in
    let source =
      match typ with
      | Cmti -> { source with cmti = true }
      | Cmt -> { source with cmt = true }
      | Cmi -> { source with cmi = true }
    in
    let srcs = StringMap.add path source t.srcs in
    { t with srcs }

type source = string * src

let sources { root; srcs } =
  List.map
    (fun (_, src) -> (root, src))
    (StringMap.bindings srcs)

let rel_xml_source (_, src) =
  let dir = match Filename.dirname src.rel with "." -> "" | p -> p in
  let base = Filename.basename src.rel in
  let xml = Filename.concat (String.capitalize base) "index.xml" in
  Filename.concat dir xml

let xml_source ((root, _) as source) =
  Filename.concat root (rel_xml_source source)

type file = {
  typ : typ;
  rel : string;
  root : string;
}

let cmti (root, { rel; cmti }) =
  if cmti then Some { typ = Cmti; rel; root }
  else None

let cmt (root, { rel; cmt }) =
  if cmt then Some { typ = Cmt; rel; root }
  else None

let cmi (root, { rel; cmi }) =
  if cmi then Some { typ = Cmi; rel; root }
  else None

let is_cmi = function
  | { typ = Cmi } -> true
  | _ -> false

let cons_opt xo xs =
  match xo with
  | None -> xs
  | Some x -> x :: xs

let files t =
  let sources = sources t in
  List.fold_left
    (fun acc src ->
       let acc = cons_opt (cmti src) acc in
       let acc = cons_opt (cmt src) acc in
       let acc = cons_opt (cmi src) acc in
       acc)
    []
    sources

let file ?(root="") path =
  match classify_path path with
  | Some (typ, rel) -> Some { typ; rel; root }
  | None -> None

let rel_path file =
  match file.typ with
  | Cmti -> file.rel ^ ".cmti"
  | Cmt -> file.rel ^ ".cmt"
  | Cmi -> file.rel ^ ".cmi"

let path file = Filename.concat file.root (rel_path file)

let rel_xml_file file =
  let dir = match Filename.dirname file.rel with "." -> "" | p -> p in
  let base = Filename.basename file.rel in
  let xml = Filename.concat (String.capitalize base) "index.xml" in
  Filename.concat dir xml

let xml_file file =
  Filename.concat file.root (rel_xml_file file)

let relocate f file =
  let root = f file.root in
    { file with root }

let summarize t =
  let cmti, cmt, cmi =
    StringMap.fold
      (fun _ source (cmti, cmt, cmi) ->
         let cmti = if source.cmti then cmti + 1 else cmti in
         let cmt = if source.cmt then cmt + 1 else cmt in
         let cmi = if source.cmi then cmi + 1 else cmi in
         (cmti, cmt, cmi))
      t.srcs (0, 0, 0)
  in
  Printf.sprintf
    "%4d cmti %4d cmt %4d cmi under %s"
    cmti cmt cmi t.root

let read root_fn file =
  let read_fn =
    match file with
    | { typ = Cmti } -> DocOck.read_cmti
    | { typ = Cmi  } -> DocOck.read_cmi
    | { typ = Cmt  } -> DocOck.read_cmt
  in
  read_fn root_fn (path file)
