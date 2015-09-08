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

val global_option_section : string

val help_sections : Cmdliner.Manpage.block list

val map_ret :
  ('a -> 'b) -> [< `Ok of 'a | `Error of bool * string ] ->
  [> `Ok of 'b | `Error of bool * string ]

module Common : sig
  type t = { force : bool; index : bool; }
  val create : force:bool -> index:bool -> t
  val force_arg : bool Cmdliner.Term.t
  val index_arg : bool Cmdliner.Term.t
  val term : t Cmdliner.Term.t
end

type path = [ `Dir of string | `File of string | `Missing of string ]
val path :
  doc:string ->
  (string option Cmdliner.Arg.converter ->
   'a option -> Cmdliner.Arg.info -> string option Cmdliner.Arg.t) ->
  path Cmdliner.Term.t
val path_opt : doc:string -> string list -> path option Cmdliner.Term.t

val output : path option Cmdliner.Term.t

val package : string Cmdliner.Term.t

val scheme : string Cmdliner.Term.t

val uri_ref : doc:string -> string list -> Uri.t option Cmdliner.Term.t

val share_dir : string Cmdliner.Term.t

module Error : sig
  val source_missing : string -> [> `Error of bool * string ]

  val source_not_found : string -> [> `Error of bool * string ]

  val use_force : string -> [> `Error of bool * string ]

  val dir_to_file : string -> string -> [> `Error of bool * string ]

  val index_to_file : string -> string -> [> `Error of bool * string ]

  val unknown_file_type : string -> [> `Error of bool * string ]

  val not_an_index : string -> [> `Error of bool * string ]

  val not_an_interface : string -> [> `Error of bool * string ]

  val wrong_version_interface : string -> [> `Error of bool * string ]

  val corrupted_interface : string -> [> `Error of bool * string ]

  val not_a_typedtree : string -> [> `Error of bool * string ]

  val no_file_package : [> `Error of bool * string ]

  val no_file_index : [> `Error of bool * string ]
end

val combine_errors :
  [< `Error of bool * string ] list -> [> `Error of bool * string ]
