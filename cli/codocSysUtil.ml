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

type file_type = Interface | Index | Unknown

let (/) = Filename.concat

let page_size = 4096

let copy in_file out_file =
  let ic = open_in_bin  in_file in
  let oc = open_out_bin out_file in
  let buf = Bytes.create page_size in
  let rec copy_more () =
    match input ic buf 0 page_size with
    | 0 -> ()
    | len -> output oc buf 0 len; copy_more ()
  in
  copy_more ();
  close_in ic;
  close_out oc

let rec read_files acc dh =
  match
    try Some (Unix.readdir dh)
    with End_of_file -> None
  with Some file -> read_files (file::acc) dh | None -> acc

let rec all_files base acc dh =
  let files = read_files [] dh in
  List.fold_left (fun acc -> function
  | "." | ".." -> acc
  | dirent ->
    let file = Filename.concat base dirent in
    try
      let dh = Unix.opendir file in
      let acc = all_files file acc dh in
      Unix.closedir dh;
      acc
    with
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> file::acc
    | Unix.Unix_error (Unix.ENOENT,  _, _) -> (* dangling symlink or race *)
      acc
  ) acc files

let in_dir path f =
  let cwd = Unix.getcwd () in
  Unix.chdir path;
  try let r = f () in Unix.chdir cwd; r
  with e -> Unix.chdir cwd; raise e

let foldp_paths f p acc dir =
  let dh = Unix.opendir dir in
  let files = in_dir dir (fun () -> all_files "" [] dh) in
  let () = Unix.closedir dh in
  List.fold_left (fun acc file ->
    if p file dir then f acc file else acc
  ) acc files

module Dir = struct
  module Error = struct
    let nondirectory_segment path =
      `Error (false, "path "^path^" is not a directory")
  end

  let rec make_exist ~perm path =
    try Unix.access path []; None
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      let dir = Filename.dirname path in
      begin match make_exist ~perm dir with
      | None ->
        Unix.(mkdir path perm);
        None
      | Some err -> Some err
      end
    | Unix.Unix_error (Unix.ENOTDIR, _, _) ->
      Some (Error.nondirectory_segment path)

  let make_dirs_exist ~perm =
    List.fold_left (fun err_opt path ->
      match err_opt with None -> make_exist ~perm path | Some err -> Some err
    ) None

  let name path = match Filename.dirname path with "." -> "" | p -> p
end

let deduce_file_type path =
  let ic = open_in path in
  let xml = Xmlm.make_input (`Channel ic) in
  let rec get_type () = match Xmlm.input xml with
    | `El_start (("","unit"),_) -> Interface
    | `El_start (("","doc-index"),_) -> Index
    | `El_start _ | `El_end | `Data _ -> Unknown
    | `Dtd _ -> get_type ()
  in
  let typ = get_type () in
  close_in ic;
  typ

let find_index_source in_dir rel_index =
  let path = in_dir / rel_index in
  if Sys.file_exists path
  then Some (path, deduce_file_type path)
  else None

(* TODO: How related to CodocConfig? *)
let default_index = "index.xml"

let search_for_source in_dir = find_index_source in_dir default_index
