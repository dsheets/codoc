open Assemblage

let version = "0.1.0"

let opam_lib = pkg "opam-lib"
let opam_units = pkg "opam-units"
let opam_doc_base = pkg "opam-doc-base"
let cow_pp = pkg_pp "cow.syntax"
let cow = pkg "cow"

let cmdliner = pkg "cmdliner"
let webmaster_cli = pkg "webmaster.cli"
let webmaster = pkg "webmaster"

let library = `Path ["lib"]
let cli = `Path ["cli"]

let ocamlary = unit "ocamlary" ~deps:[
  opam_lib;
  opam_units;
  opam_doc_base;
] library

let ocamlary_doc_html = unit "ocamlaryDocHtml" ~deps:[
  opam_doc_base;
  cow_pp;
  cow;
] library

let ocamlary = lib "ocamlary" (`Units [ocamlary; ocamlary_doc_html])

let ocamlary_cli = unit "ocamlaryCli" ~deps:[cmdliner] cli

let ocamlary_doc = unit "ocamlaryDoc" ~deps:[
  cow_pp;
  cow;
  webmaster_cli;
  webmaster;
  ocamlary;
] cli

let ocamlary_cmd = unit "ocamlaryMain" ~deps:[
  cmdliner;
  webmaster_cli;
] cli

let bin = bin "ocamlary" (`Units [
  ocamlary_cli; ocamlary_doc; ocamlary_cmd;
])

;;

assemble (project ~version "ocamlary" [
  ocamlary;
  bin;
])
