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

val is_extractable : string -> bool

type t

val empty : root:string -> t

val add : t -> string -> t

val summarize : t -> string

type source

val sources : t -> source list

val xml_source : source -> string

val rel_xml_source : source -> string

type file

val file : ?root:string -> string -> file option

val cmti : source -> file option

val cmt : source -> file option

val cmi : source -> file option

val files : t -> file list

val is_cmi : file -> bool

val read : (string -> Digest.t -> 'a) -> file -> 'a DocOck.result

val path : file -> string

val rel_path : file -> string

val xml_file : file -> string

val rel_xml_file : file -> string

val relocate : (string -> string) -> file -> file
