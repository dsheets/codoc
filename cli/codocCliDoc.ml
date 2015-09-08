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

let generate common output path pkg scheme css share =
  let common = { common with CodocCli.Common.index = true } in
  let extract_ret = CodocCliExtract.run common output path pkg in
  match extract_ret with
  | `Error _ as err ->
    (* TODO: better error handling? *)
    Printf.eprintf "Could not extract without errors. Terminating.\n%!";
    err
  | `Ok intermediate ->
    let common = { common with CodocCli.Common.force = true } in
    let link_ret = CodocCliLink.run common output intermediate pkg in
    match link_ret with
    | `Error _ as err ->
      (* TODO: better error handling? *)
      Printf.eprintf "Could not link without errors. Terminating.\n%!";
      err
    | `Ok () ->
      let html_ret =
        CodocCliHtml.run common output intermediate pkg scheme css share
      in
      match html_ret with
      | `Error _ as err ->
        (* TODO: better error handling? *)
        Printf.eprintf "Could not render HTML without errors. Terminating.\n%!";
        err
      | `Ok () -> `Ok ()
