open Assemblage

let version = "0.1.0"

let doc_ock_lib = pkg "doc-ock-lib"
let doc_ock_xml = pkg "doc-ock-xml"

let cow_pp = pkg_pp "cow.syntax"
let cow = pkg "cow"

let findlib_units = pkg "ocamlfind-units"

let cmdliner = pkg "cmdliner"
let webmaster_cli = pkg "webmaster.cli"
let webmaster = pkg "webmaster"

let library = `Path ["lib"]
let cli = `Path ["cli"]

let ocamlary_doc_maps = unit "ocamlaryDocMaps" ~deps:[
  doc_ock_lib;
] library

let ocamlary_doc = unit "ocamlaryDoc" ~deps:[
  cow_pp;
  cow;
  doc_ock_lib;
  ocamlary_doc_maps;
] library

let ocamlary_doc_html = unit "ocamlaryDocHtml" ~deps:[
  doc_ock_lib;
  cow_pp;
  cow;
  ocamlary_doc;
] library

let ocamlary = lib "ocamlary" (`Units [
  ocamlary_doc;
  ocamlary_doc_html;
])

let ocamlary_cli = unit "ocamlaryCli" ~deps:[cmdliner] cli

let ocamlary_cli_doc = unit "ocamlaryCliDoc" ~deps:[
  cow_pp;
  cow;
  findlib_units;
  webmaster_cli;
  webmaster;
  doc_ock_xml;
  ocamlary;
] cli

let ocamlary_cmd = unit "ocamlaryMain" ~deps:[
  cmdliner;
  webmaster_cli;
  ocamlary_cli_doc;
] cli

let bin = bin "ocamlary" (`Units [
  ocamlary_cli; ocamlary_cli_doc; ocamlary_cmd;
])

;;

assemble (project ~version "ocamlary" [
  ocamlary;
  bin;
])
