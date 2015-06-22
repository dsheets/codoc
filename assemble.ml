open Assemblage

let version = "0.2.0"

let warnings = ["-w";"@f@p@u@y"]
let flags =
  Flags.(v (`Compile `Byte) warnings @@@ v (`Compile `Native) warnings)

let unix = pkg "unix"

let stringext = pkg "stringext"

let uri = pkg "uri"

let doc_ock = pkg "doc-ock"
let doc_ock_xml = pkg "doc-ock-xml"

let xmlm = pkg "xmlm"

let cow_pp = pkg_pp "cow.syntax"
let cow = pkg "cow"

let cmdliner = pkg "cmdliner"

let library = `Path ["lib"]
let cli = `Path ["cli"]
let opam = `Path ["opam-doc"]

let codoc_html = unit "codocHtml" ~deps:[
  cow_pp;
  cow;
] library

let codoc_doc_maps = unit "codocDocMaps" ~deps:[
  doc_ock;
] library

let codoc_doc = unit "codocDoc" ~deps:[
  cow_pp;
  cow;
  doc_ock;
  codoc_doc_maps;
] library

let codoc_util = unit "codocUtil" ~deps:[
  stringext;
  doc_ock;
  codoc_doc;
] library

let codoc_doc_html = unit "codocDocHtml" ~deps:[
  doc_ock;
  cow_pp;
  cow;
  codoc_doc;
  codoc_html;
] library

let codoc_xml = unit "codocXml" ~deps:[
  xmlm;
  doc_ock_xml;
  codoc_doc;
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

let codoc_env = unit "codocEnvironment" ~deps:[
  doc_ock_xml;
  codoc_doc;
] library

let codoc = lib ~flags "codoc" (`Units [
  codoc_html;
  codoc_doc_maps;
  codoc_doc;
  codoc_util;
  codoc_doc_html;
  codoc_xml;
  codoc_index;
  codoc_index_html;
  codoc_env;
])

let codoc_config = unit "codocConfig" ~deps:[] cli

let codoc_sys_util = unit "codocSysUtil" ~deps:[
  xmlm;
  codoc;
] cli

let codoc_cli = unit "codocCli" ~deps:[
  uri;
  cmdliner;
  codoc_config;
] cli

let cli_lib = lib ~flags "codoc.cli" (`Units [
  codoc_cli;
])

let codoc_cli_extract = unit "codocCliExtract" ~deps:[
  codoc;
  cli_lib;
  codoc_sys_util;
  doc_ock;
  doc_ock_xml;
] cli

let codoc_cli_link = unit "codocCliLink" ~deps:[
  xmlm;
  codoc;
  cli_lib;
  codoc_sys_util;
  doc_ock;
  doc_ock_xml;
] cli

let codoc_cli_html = unit "codocCliHtml" ~deps:[
  xmlm;
  cow_pp;
  cow;
  codoc;
  cli_lib;
  codoc_sys_util;
] cli

let codoc_cli_doc = unit "codocCliDoc" ~deps:[
  cow_pp;
  cow;
  doc_ock;
  doc_ock_xml;
  codoc;
  codoc_cli_extract;
  codoc_cli_link;
  codoc_cli_html;
] cli

let codoc_cmd = unit "codocMain" ~deps:[
  cmdliner;
  cli_lib;
  codoc_cli_extract;
  codoc_cli_link;
  codoc_cli_html;
  codoc_cli_doc;
] cli

let codoc_bin = bin ~flags "codoc" (`Units [
  codoc_cli_extract;
  codoc_cli_link;
  codoc_cli_html;
  codoc_cli_doc;
  codoc_cmd;
])

let opam_doc = unit "opamDoc" ~deps:[
  unix;
  cmdliner;
  cli_lib;
  codoc_config;
] opam

let opamdoc = bin ~flags "opam-doc" (`Units [
  opam_doc;
])

;;

assemble (project ~version "codoc" [
  codoc;
  codoc_bin;
  opamdoc;
])
