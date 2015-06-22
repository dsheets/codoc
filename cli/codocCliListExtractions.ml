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
module Dir = CodocSysUtil.Dir

let (/) = Filename.concat

let only_cmti file path =
  if Filename.check_suffix file ".cmti"
  then begin
    true
  end
  else false

let all_cmtis dir =
  CodocSysUtil.foldp_paths (fun lst rel_cmti -> rel_cmti::lst) only_cmti [] dir

let list = all_cmtis

let run = function
  | `Missing path -> Error.source_missing path
  | `File in_file when not (Filename.check_suffix in_file ".cmti") ->
    `Error (false, "source "^in_file^" is not a cmti")
  | `File in_file -> print_endline in_file; `Ok ()
  | `Dir in_dir ->
    let extractions = list in_dir in
    List.iter print_endline extractions;
    `Ok ()
