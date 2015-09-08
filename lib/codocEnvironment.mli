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

(** [CodocEnvironment] describes a linking environment for
    documentation reference resolution. *)

(** [t] is the type of a [CodocEnvironment]. *)
type t

(** [root_by_name env name] is the root corresponding to a given
    compilation unit [name] in the environment [env]. If [env]
    contains no compilation unit by [name], [None] is returned.

    @raise {!CodocXml.ParseError} when reading the appropriate
    compilation unit for its root identifier fails due to an XML error.
*)
val root_by_name : t -> string -> CodocDoc.root option

val unit_by_root : t -> CodocDoc.root -> CodocDoc.root DocOck.Types.Unit.t option

val unit_by_name : t -> string -> CodocDoc.root DocOck.Types.Unit.t option

val path_by_root : t -> CodocDoc.root -> string

val index :
  t ->
  string ->
  string -> CodocDoc.root -> CodocDoc.root DocOck.Types.Unit.t -> unit

val index_units : t -> CodocIndex.t -> t

val create : CodocIndex.t -> t

val create_for_unit : CodocDoc.root DocOck.Types.Unit.t -> t

val resolver : t -> CodocDoc.root DocOck.resolver

val expander : t -> CodocDoc.root DocOck.expander

val failures_of_root : t -> CodocDoc.root -> string list
