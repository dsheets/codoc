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

module StringSet = Set.Make(String)

type 'a r = {
  cmti : 'a;
  cmi  : 'a;
  cmt  : 'a;
}

type typ = Cmti | Cmi | Cmt

type file = {
  typ : typ;
  rel : string;
  src : string;
}

type t = {
  root : string;
  set  : StringSet.t r;
}

type env = t

let at root =
  StringSet.({ root; set = { cmti = empty; cmi = empty; cmt = empty; }; })

let add_ set el = StringSet.add el set

let add_cmti ({ cmti } as x) path = { x with cmti = add_ cmti path }
let add_cmi  ({ cmi  } as x) path = { x with cmi  = add_ cmi  path }
let add_cmt  ({ cmt  } as x) path = { x with cmt  = add_ cmt  path }

let file ?(src="") rel_file = Filename.(
  if check_suffix rel_file ".cmti"
  then Some { typ = Cmti; rel = chop_suffix rel_file ".cmti"; src; }
  else if check_suffix rel_file ".cmi"
  then Some { typ = Cmi;  rel = chop_suffix rel_file ".cmi";  src; }
  else if check_suffix rel_file ".cmt"
  then Some { typ = Cmt;  rel = chop_suffix rel_file ".cmt";  src; }
  else None
)

let is_extractable path = Filename.(
  check_suffix path ".cmti" ||
  check_suffix path ".cmt"  ||
  check_suffix path ".cmi"
)

let filter { root; set = { cmti; cmi; cmt }; } = StringSet.({
  root;
  set = {
    cmti;
    cmt = diff cmt cmti;
    cmi = diff (diff cmi cmti) cmt;
  };
})

let fold f acc { root; set = { cmti; cmi; cmt }; } =
  let list = StringSet.fold (f.cmti root) cmti acc in
  let list = StringSet.fold (f.cmi  root) cmi list in
  StringSet.fold (f.cmt root) cmt list

let map f = fold {
  cmti = (fun root v list -> f.cmti root v :: list);
  cmi  = (fun root v list -> f.cmi  root v :: list);
  cmt  = (fun root v list -> f.cmt  root v :: list);
}

let apply f = function
  | { typ = Cmti; src; rel; } -> f.cmti src rel
  | { typ = Cmt;  src; rel; } -> f.cmt  src rel
  | { typ = Cmi;  src; rel; } -> f.cmi  src rel

let mapply a f = function
  | { typ = Cmti; rel; } -> { a with cmti = f.cmti rel a.cmti }
  | { typ = Cmt;  rel; } -> { a with cmt  = f.cmt  rel a.cmt  }
  | { typ = Cmi;  rel; } -> { a with cmi  = f.cmi  rel a.cmi  }

let add_f = StringSet.({ cmti = add; cmt = add; cmi = add; })

let add extr next = match file next with
  | None -> extr
  | Some file -> { extr with set = mapply extr.set add_f file }

let rel_cmti _ path = path ^ ".cmti"
let rel_cmt  _ path = path ^ ".cmt"
let rel_cmi  _ path = path ^ ".cmi"

let cmti root path = Filename.concat root (rel_cmti root path)
let cmt  root path = Filename.concat root (rel_cmt  root path)
let cmi  root path = Filename.concat root (rel_cmi  root path)

let rel_path_f = { cmti = rel_cmti; cmt = rel_cmt; cmi = rel_cmi; }

let path_f = { cmti; cmt; cmi; }

let uniform_cons f = {
  cmti = (fun src rel -> f { typ = Cmti; rel; src; });
  cmi  = (fun src rel -> f { typ = Cmi;  rel; src; });
  cmt  = (fun src rel -> f { typ = Cmt;  rel; src; });
}

let file_f = uniform_cons (fun x -> x)

let rel_xml_path _ p =
  let dir = match Filename.dirname p with "." -> "" | p -> p in
  let xml = Filename.(concat (String.capitalize (basename p)) "index.xml") in
  Filename.concat dir xml

let xml_path root p = Filename.concat root (rel_xml_path root p)

let uniform f = { cmti = f; cmi = f; cmt = f; }

let uapply f = apply (uniform_cons (fun file ->
  let src, rel = f file.src file.rel in
  { file with src; rel }
))

let xml_f = uniform xml_path

let xml = apply xml_f

let rel_xml_f = uniform rel_xml_path

let rel_xml = apply rel_xml_f

let path = apply path_f

let rel_path = apply rel_path_f

let relocate src = apply (uniform_cons (fun x -> { x with src }))

let path_list = map path_f []

let file_list = map file_f []

let xml_list = map xml_f []

let summarize { root; set } =
  let cmti_count = StringSet.cardinal set.cmti in
  let cmi_count  = StringSet.cardinal set.cmi  in
  let cmt_count  = StringSet.cardinal set.cmt  in
  Printf.sprintf
    "%4d cmti %4d cmi %4d cmt under %s" cmti_count cmi_count cmt_count root

let read root_fn file =
  let read_fn = match file with
    | { typ = Cmti } -> DocOck.read_cmti
    | { typ = Cmi  } -> DocOck.read_cmi
    | { typ = Cmt  } -> DocOck.read_cmt
  in
  read_fn root_fn (path file)
