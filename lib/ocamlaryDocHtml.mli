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
  index_depth:int -> doc_base:Uri.t -> OpamDocPath.parent -> pathloc

val name_of_library : OpamDocTypes.library -> string
val libraries : Ocamlary.state -> OpamPackage.t -> OpamDocTypes.library list

val of_top_module :
  OpamDocState.state -> pathloc:pathloc -> OpamDocTypes.module_ -> Cow.Html.t

val index_of_library : doc_base:Uri.t -> OpamDocTypes.library -> Cow.Html.t
val widget_of_libraries :
  doc_base:Uri.t -> OpamDocTypes.library list -> Cow.Html.t
