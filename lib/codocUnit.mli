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

open DocOckTypes
open CodocDoc

module Id : sig
  val name_of_argument : int -> string -> string
end

module Substruct : sig
  type ('a, 'b) map = {
    map_class : 'a -> root Class.t -> 'b;
    map_classtype : 'a -> root ClassType.t -> 'b;
    map_module : 'a -> root Module.t -> 'b;
    map_moduletype : 'a -> root ModuleType.t -> 'b;
  }

  type 'a collect = ('a, 'a) map

  type 'a t =
    | Class of root Class.t * 'a
    | ClassType of root ClassType.t * 'a
    | Module of root Module.t * 'a t list * 'a
    | ModuleType of root ModuleType.t * 'a t list * 'a

  type 'a name =
    | ClassName of string * 'a
    | ClassTypeName of string * 'a
    | ModuleName of string * 'a name list * 'a
    | ModuleTypeName of string * 'a name list * 'a

  val root_of_unit_signature :
    root Unit.t -> root Signature.t -> unit t

  val map_of_unit :
    (unit, 'a) map -> root Unit.t -> 'a t option

  val map : ('a, 'b) map -> 'a t -> 'b t

  val apply : ('a, 'b) map -> 'a t -> 'b

  val fold : (('a * 'b), 'a) map -> 'a -> 'b t -> 'a

  val compose : ('a, 'b) map -> ('b, 'c) map -> ('a, 'c) map

  val product : ('a, 'b) map -> ('a, 'c) map -> ('a, ('b * 'c)) map

  val homo_map : ('a -> 'b) -> ('a, 'b) map

  val ident_map : ('a, root CodocDocMaps.Identifier.parent) map

  val list_option_fold : unit -> (('a list * 'a option), 'a list) map

  val to_name : 'a t -> 'a name

  val string_of_name : 'a name -> string
end

module Href : sig
  type loc

  val html_name_of : string -> string
  val uri_of_path : scheme:string -> string -> Uri.t
  val normal_uri_for_scheme : string -> (Uri.t -> Uri.t)

  val loc : ?pkg_root:string -> string -> 'a Substruct.t -> loc option

  val up : loc -> Uri.t option

  val of_ident :
    loc -> root CodocDocMaps.Identifier.any -> Uri.t option

  val id_of_ident :
    loc -> root CodocDocMaps.Identifier.any -> string option

  val of_name : 'a Substruct.name -> Uri.t

  val ascent_of_ident : root CodocDocMaps.Identifier.any -> string
end
