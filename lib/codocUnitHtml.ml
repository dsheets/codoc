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

let of_class scheme pkg_root _ c =
  let sub = CodocUnit.Substruct.Class (c,()) in
  match CodocUnit.(Href.loc ?pkg_root scheme sub) with
  | None -> failwith "invariant violation render_class/loc" (* TODO: ? *)
  | Some loc -> CodocDocHtml.of_top_class loc c

let of_classtype scheme pkg_root _ c =
  let sub = CodocUnit.Substruct.ClassType (c,()) in
  match CodocUnit.(Href.loc ?pkg_root scheme sub) with
  | None -> failwith "invariant violation render_classtype/loc" (* TODO: ? *)
  | Some loc -> CodocDocHtml.of_top_classtype loc c

let of_module scheme pkg_root _ m =
  let sub = CodocUnit.Substruct.Module (m,[],()) in
  match CodocUnit.(Href.loc ?pkg_root scheme sub) with
  | None -> failwith "invariant violation render_module/loc" (* TODO: ? *)
  | Some loc -> CodocDocHtml.of_top_module loc m

let of_moduletype scheme pkg_root _ m =
  let sub = CodocUnit.Substruct.ModuleType (m,[],()) in
  match CodocUnit.(Href.loc ?pkg_root scheme sub) with
  | None -> failwith "invariant violation render_moduletype/loc" (* TODO: ? *)
  | Some loc -> CodocDocHtml.of_top_moduletype loc m

let of_substruct_map scheme pkg_root =
  CodocUnit.Substruct.({
    map_class = of_class scheme pkg_root;
    map_classtype = of_classtype scheme pkg_root;
    map_module = of_module scheme pkg_root;
    map_moduletype = of_moduletype scheme pkg_root;
  })

let of_substruct scheme pkg_root sub =
  CodocUnit.Substruct.apply (of_substruct_map scheme pkg_root) sub

let of_substructs scheme pkg_root sub =
  CodocUnit.Substruct.map (of_substruct_map scheme pkg_root) sub
