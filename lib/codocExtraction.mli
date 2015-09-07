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

type file
type env
type t

type 'a r = {
  cmti : 'a;
  cmi  : 'a;
  cmt  : 'a;
}

val file : ?src:string -> string -> file option
val is_extractable : string -> bool

val at : string -> env

val filter : env -> t

val apply : (string -> string -> 'a) r -> file -> 'a

val uapply : (string -> string -> string * string) -> file -> file

val add : env -> string -> env

val path : file -> string

val rel_path : file -> string

val xml : file -> string

val rel_xml : file -> string

val relocate : string -> file -> file

val path_list : t -> string list
val file_list : t -> file list
val xml_list  : t -> string list

val summarize : t -> string

val read : (string -> Digest.t -> 'a) -> file -> 'a DocOck.result
