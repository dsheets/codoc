open Assemblage

let version = "0.1.0"

let uri = pkg "uri"

let doc_ock_lib = pkg "doc-ock-lib"
let doc_ock_xml = pkg "doc-ock-xml"

let cow_pp = pkg_pp "cow.syntax"
let cow = pkg "cow"

let cmdliner = pkg "cmdliner"
let webmaster_cli = pkg "webmaster.cli"
let webmaster = pkg "webmaster"

let library = `Path ["lib"]
let cli = `Path ["cli"]

let codoc_util = unit "codocUtil" ~deps:[] library

let codoc_html = unit "codocHtml" ~deps:[
  cow_pp;
  cow;
] library

let codoc_doc_maps = unit "codocDocMaps" ~deps:[
  doc_ock_lib;
] library

let codoc_doc = unit "codocDoc" ~deps:[
  cow_pp;
  cow;
  doc_ock_lib;
  codoc_doc_maps;
] library

let codoc_doc_html = unit "codocDocHtml" ~deps:[
  doc_ock_lib;
  cow_pp;
  cow;
  codoc_doc;
  codoc_html;
] library

let codoc_index = unit "codocIndex" ~deps:[
  cow_pp;
  cow;
] library

let codoc_index_html = unit "codocIndexHtml" ~deps:[
  cow_pp;
  cow;
  codoc_util;
  codoc_html;
] library

let codoc = lib "codoc" (`Units [
  codoc_index;
  codoc_index_html;
  codoc_doc;
  codoc_doc_html;
])

let codoc_config = unit "codocConfig" ~deps:[] cli

let codoc_cli = unit "codocCli" ~deps:[
  uri;
  cmdliner;
  webmaster_cli;
  codoc_config;
] cli

let codoc_cli_doc = unit "codocCliDoc" ~deps:[
  cow_pp;
  cow;
  webmaster_cli;
  webmaster;
  doc_ock_lib;
  doc_ock_xml;
  codoc;
] cli

let codoc_cmd = unit "codocMain" ~deps:[
  cmdliner;
  webmaster_cli;
  codoc_cli_doc;
] cli

let bin = bin "codoc" (`Units [
  codoc_cli; codoc_cli_doc; codoc_cmd;
])

;;

assemble (project ~version "codoc" [
  codoc;
  bin;
])
