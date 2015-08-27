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

type pathloc

val pathloc :
  unit:CodocDoc.root DocOckTypes.Unit.t ->
  index:(CodocDoc.root -> Uri.t option) -> ?pkg_root:string ->
  normal_uri:(Uri.t -> Uri.t) -> pathloc
(** Create a path location value for relative linking. *)

val of_top_module :
  pathloc:pathloc -> CodocDoc.root DocOckTypes.Module.t -> Blueprint.t
(** Generate a documentation page from a module. *)

val of_unit :
  pathloc:pathloc -> CodocDoc.root DocOckTypes.Unit.t -> Blueprint.t
(** Generate a documentation page from a compilation unit. *)
