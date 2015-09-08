(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

val of_top_module :
  loc:CodocUnit.Href.loc -> CodocDoc.root DocOckTypes.Module.t -> Blueprint.t
(** Generate a documentation page from a module. *)

val of_top_moduletype :
  loc:CodocUnit.Href.loc -> CodocDoc.root DocOckTypes.ModuleType.t -> Blueprint.t
(** Generate a documentation page from a module type. *)

val of_top_class :
  loc:CodocUnit.Href.loc -> CodocDoc.root DocOckTypes.Class.t -> Blueprint.t
(** Generate a documentation page from a class. *)

val of_top_classtype :
  loc:CodocUnit.Href.loc -> CodocDoc.root DocOckTypes.ClassType.t -> Blueprint.t
(** Generate a documentation page from a class type. *)

val of_top_unit :
  loc:CodocUnit.Href.loc -> CodocDoc.root DocOckTypes.Unit.t -> Blueprint.t
(** Generate a documentation page from a unit. *)
