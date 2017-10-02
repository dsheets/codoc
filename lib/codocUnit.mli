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
open CodocDoc

module Id : sig
  val name_of_argument : int -> string -> string
end

module Href : sig
  type loc

  val html_name_of : string -> string
  val uri_of_path : scheme:string -> string -> Uri.t
  val normal_uri_for_scheme : string -> (Uri.t -> Uri.t)

  val loc : ?pkg_root:string -> ?base:string -> string ->
            CodocDoc.root DocOck.Paths.Identifier.any -> loc option

  val up : loc -> Uri.t option

  val of_ident :
    loc -> root CodocDocMaps.Identifier.any -> Uri.t option

  val id_of_ident :
    loc -> root CodocDocMaps.Identifier.any -> string option

  val ascent_of_ident : root CodocDocMaps.Identifier.any -> string
end
