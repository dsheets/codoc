
module StringMap = Map.Make(String)

type generation_issue =
| Module_resolution_failed of string
| Xml_error of string * string (* TODO: start/pos *)

type generated_unit = {
  mod_name  : string;
  xml_file  : string;
  html_file : string option;
  issues    : generation_issue list;
}

type pkg = {
  pkg_name : string;
  index    : string;
}

type t = {
  pkgs  : pkg StringMap.t;
  units : generated_unit StringMap.t;
}

let list_of_map map = List.rev_map snd (StringMap.bindings map)

let index_filename = "index.xml"

let index_file dir = Filename.concat dir index_filename

let xml_of_generation_issue = function
  | Module_resolution_failed mod_name ->
    <:xml<<resolution-failed module=$str:mod_name$/>&>>
  | Xml_error (xml_file, msg) ->
    (* TODO: start/pos *)
    <:xml<<xml-error href=$str:xml_file$>$str:msg$</xml-error>&>>

let xml_of_generated_unit ({ mod_name; xml_file; html_file; issues }) =
  let issues = match issues with
    | [] -> <:xml<&>>
    | issues -> <:xml<<issues>
      $list:List.map xml_of_generation_issue issues$
    </issues>&>>
  in
  let html_file = match html_file with
    | Some html_file -> <:xml<<file type="text/html" href=$str:html_file$/>&>>
    | None -> <:xml<&>>
  in
  <:xml<
  <unit name=$str:mod_name$>
    <file type="application/xml" href=$str:xml_file$/>
    $html_file$
    $issues$
  </unit>&>>

let xml_of_pkg ({ pkg_name; index }) =
  <:xml<<package name=$str:pkg_name$ href=$str:index$/>&>>

let to_xml ({ units; pkgs }) =
  <:xml<<doc-index>
  $list:list_of_map (StringMap.map xml_of_pkg pkgs)$
  $list:list_of_map (StringMap.map xml_of_generated_unit units)$
</doc-index>&>>

let string_of_pos (line,col) =
  Printf.sprintf "line %d, col %d" line col

let rec must_end xml = match Xmlm.input xml with
  | `El_end -> ()
  | `Data _ -> must_end xml (* TODO: ? *)
  | _ -> (* TODO: fixme *)
    failwith (Printf.sprintf "expected end: %s" (string_of_pos (Xmlm.pos xml)))

let eat xml = ignore (Xmlm.input xml)

let just_data xml = match Xmlm.peek xml with
  | `Data data -> eat xml; data
  | _ -> (* TODO: fixme *) failwith "expected data"

let rec generation_issue_of_xml xml = match Xmlm.peek xml with
  | `El_start (("","resolution-failed"),[("","module"),name]) ->
    eat xml;
    must_end xml;
    Module_resolution_failed name
  | `El_start (("","xml-error"),[("","href"),href]) ->
    eat xml;
    let message = just_data xml in
    must_end xml;
    Xml_error (href, message)
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
  | `El_end -> (* TODO: fixme *) failwith "unexpected end"
  | `Data _ | `Dtd _ -> eat xml; generation_issue_of_xml xml

let rec issues_of_xml xml issues = match Xmlm.peek xml with
  | `El_start _ ->
    let issue = generation_issue_of_xml xml in
    issues_of_xml xml (issue::issues)
  | `El_end -> eat xml; issues
  | `Dtd _ | `Data _ -> eat xml; issues_of_xml xml issues

let rec inside xml tag fn = match Xmlm.peek xml with
  | `El_start (el,attrs) when el = tag ->
    eat xml;
    fn attrs
  | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
  | `El_end -> []
  | `Data _ | `Dtd _ -> eat xml; inside xml tag fn

let rec files_of_xml xml files = match Xmlm.peek xml with
  | `El_start (("","file"),[("","type"),typ;("","href"),href])
  | `El_start (("","file"),[("","href"),href;("","type"),typ]) ->
    eat xml;
    must_end xml;
    files_of_xml xml ((typ,href)::files)
  | `El_start _ | `El_end -> files
  | `Dtd _ | `Data _ -> eat xml; files_of_xml xml files

let generated_unit_of_xml xml mod_name =
  let files = files_of_xml xml [] in
  let xml_file = List.assoc "application/xml" files in
  let html_file = Some (List.assoc "text/html" files) in
  let issues = inside xml ("","issues") (fun _ -> issues_of_xml xml []) in
  must_end xml;
  { mod_name; xml_file; html_file; issues }

let pkg_of_xml xml pkg_name index =
  must_end xml;
  { pkg_name; index }

let empty = { units = StringMap.empty; pkgs = StringMap.empty; }

let rec of_xml xml =
  let rec doc_index index = match Xmlm.input xml with
      | `El_start (("","unit"),[("","name"), name]) ->
        doc_index { index with
          units = StringMap.add name (generated_unit_of_xml xml name)
            index.units
        }
      | `El_start (("","package"),[("","name"), name; ("","href"), href])
      | `El_start (("","package"),[("","href"), href; ("","name"), name]) ->
        doc_index { index with
          pkgs = StringMap.add name (pkg_of_xml xml name href) index.pkgs
        }
      | `El_start _ -> (* TODO: fixme *) failwith "unknown element"
      | `El_end -> index
      | `Data _ | `Dtd _ -> doc_index index
  in
  let start = function
    | (("","doc-index"),_) -> doc_index empty
    | _ -> (* TODO: fixme *) failwith "unknown root node"
  in
  match Xmlm.input xml with
  | `El_start tag -> start tag
  | `El_end -> empty
  | `Data _ | `Dtd _ -> of_xml xml

let read dir =
  let file = index_file dir in
  if Sys.file_exists file
  then
    let ic = open_in file in
    let input = Xmlm.make_input (`Channel ic) in
    let index = of_xml input in
    let () = close_in ic in
    index
  else empty

let write dir index =
  let file = index_file dir in
  let oc = open_out file in
  let output = Xmlm.make_output (`Channel oc) in
  Xmlm.output output (`Dtd None);
  Xmlm.output_tree (fun x -> x) output (List.hd (to_xml index));
  close_out oc
