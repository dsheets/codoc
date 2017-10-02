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

module Error = CodocCli.Error

let run = function
  | `Missing path -> Error.source_missing path
  | `File in_file when not (CodocExtraction.is_extractable in_file) ->
    `Error (false, "source "^in_file^" is not cmti, cmt, or cmi")
  | `File in_file -> print_endline in_file; `Ok ()
  | `Dir in_dir ->
    let extractions = CodocSysUtil.collect in_dir in
    let files = CodocExtraction.files extractions in
    let paths = List.map CodocExtraction.path files in
    List.iter print_endline paths;
    `Ok ()
