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

val of_module :
  loc:CodocUnit.Href.loc -> CodocEnvironment.t ->
    CodocDoc.root DocOck.Types.Module.t -> Blueprint.t
(** Generate a documentation page from a module. *)

val of_module_type :
  loc:CodocUnit.Href.loc -> CodocEnvironment.t ->
    CodocDoc.root DocOck.Types.ModuleType.t -> Blueprint.t
(** Generate a documentation page from a module type. *)

val of_argument :
  loc:CodocUnit.Href.loc -> CodocEnvironment.t ->
    CodocDoc.root DocOck.Paths.Identifier.module_
    * CodocDoc.root DocOck.Types.ModuleType.expr -> Blueprint.t
(** Generate a documentation page from a module. *)

val of_class :
  loc:CodocUnit.Href.loc -> CodocEnvironment.t ->
    CodocDoc.root DocOck.Types.Class.t -> Blueprint.t
(** Generate a documentation page from a class. *)

val of_class_type :
  loc:CodocUnit.Href.loc -> CodocEnvironment.t ->
    CodocDoc.root DocOck.Types.ClassType.t -> Blueprint.t
(** Generate a documentation page from a class type. *)

val of_unit :
  loc:CodocUnit.Href.loc -> CodocEnvironment.t ->
    CodocDoc.root DocOck.Types.Unit.t -> Blueprint.t
(** Generate a documentation page from a unit. *)
