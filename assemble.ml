open Assemblage

let version = "0.2.0"

let warnings = ["-w";"@f@p@u@y"]
let flags =
  Flags.(v (`Compile `Byte) warnings @@@
         v (`Compile `Native) warnings @@@
         v (`Link `Native) [(*"-p"*)])

let unix = pkg "unix"

let stringext = pkg "stringext"

let uri = pkg "uri"

let doc_ock = pkg "doc-ock"
let doc_ock_xml = pkg "doc-ock-xml"

let xmlm = pkg "xmlm"

let blueprint = pkg "blueprint"

let cmdliner = pkg "cmdliner"

let library = `Path ["lib"]
let cli = `Path ["cli"]
let opam = `Path ["opam-doc"]

let codoc_doc_maps = unit "codocDocMaps" ~deps:[
  doc_ock;
] library

let codoc_extraction = unit "codocExtraction" ~deps:[
  doc_ock;
] library

let codoc_doc = unit "codocDoc" ~deps:[
  doc_ock;
  doc_ock_xml;
  codoc_doc_maps;
  codoc_extraction;
] library

let codoc_util = unit "codocUtil" ~deps:[
  stringext;
  doc_ock;
  codoc_doc;
] library

let codoc_unit = unit "codocUnit" ~deps:[
  uri;
  doc_ock;
  codoc_util;
  codoc_doc;
  codoc_doc_maps;
] library

let codoc_analysis = unit "codocAnalysis" ~deps:[
  xmlm;
  doc_ock;
] library

let codoc_doc_html = unit "codocDocHtml" ~deps:[
  uri;
  doc_ock;
  blueprint;
  codoc_doc;
] library

let codoc_xml = unit "codocXml" ~deps:[
  xmlm;
  doc_ock_xml;
  codoc_doc;
] library

let codoc_index = unit "codocIndex" ~deps:[
  xmlm;
  stringext;
] library

let codoc_index_html = unit "codocIndexHtml" ~deps:[
  uri;
  blueprint;
  codoc_util;
] library

let codoc_env = unit "codocEnvironment" ~deps:[
  doc_ock_xml;
  codoc_doc;
] library

let codoc_template = unit "codocTemplate" ~deps:[
  blueprint;
  xmlm;
  unix; (* TODO: would be nice out of the library and into the tool *)
] library

let codoc = lib ~flags "codoc" (`Units [
  codoc_doc_maps;
  codoc_extraction;
  codoc_doc;
  codoc_util;
  codoc_unit;
  codoc_xml;
  codoc_analysis;
  codoc_index;
  codoc_env;
  codoc_doc_html;
  codoc_index_html;
  codoc_template;
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

let cli_lib = lib ~flags "cli" (`Units [
  codoc_cli;
])

let codoc_cli_list_extractions = unit "codocCliListExtractions" ~deps:[
  cli_lib;
  codoc_sys_util;
  codoc;
] cli

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
  blueprint;
  codoc;
  cli_lib;
  codoc_sys_util;
] cli

let codoc_cli_doc = unit "codocCliDoc" ~deps:[
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
  codoc_cli_list_extractions;
  codoc_cli_extract;
  codoc_cli_link;
  codoc_cli_html;
  codoc_cli_doc;
] cli

let codoc_bin = bin ~flags "codoc" (`Units [
  codoc_cli_list_extractions;
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
