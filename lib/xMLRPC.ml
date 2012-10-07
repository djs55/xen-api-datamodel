(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(*
exception RunTimeTypeError of string * Xml.xml

let rtte name xml =
	error "Error: name='%s'; xml= %s" name (String.escaped (Xml.to_string xml));
	raise (RunTimeTypeError(name, xml))

type xmlrpc = Xml.xml

let pretty_print = function
	| Xml.Element(tag,_,_) -> "Element=" ^ String.escaped tag
	| Xml.PCData d         -> "PCData=" ^ String.escaped d
*)

type writer = Xmlm.output -> unit

type response =
  | Success of writer                 (** normal result *)
  | Failure of string * (string list) (** failure/ exception in high-level code *)
  | Fault   of (int32 * string)       (** error in the XMLRPC handling *)
  | Raw     of writer                 (** Skipping the status *)


module ToString = struct
  let int64 = Int64.to_string
  let double = Printf.sprintf "%0.16g"
  let string x = x
end

module FromString = struct
  let int64 = Int64.of_string
  let double = float_of_string
  let string x = x
end

let to_string f =
	let b = Buffer.create 128 in
	let o = Xmlm.make_output ~indent:(Some 4) (`Buffer b) in
	Xmlm.output o (`Dtd None);
	let () = f o in
	Buffer.contents b

module To = struct
  let pcdata string o = Xmlm.output o (`Data string)

  let box tag f o =
	  Xmlm.output o (`El_start (("", tag), []));
	  f o;
	  Xmlm.output o `El_end

  let value = box "value"

  let nothing o = ()

  let nil = value (box "nil" nothing)

  let name v = box "name" (pcdata v)

  let array f = value (box "array" (box "data" f))

  let boolean b = value (box "boolean" (pcdata (if b then "1" else "0")))

  let datetime s = value (box "dateTime.iso8601" (pcdata s))

  let double x =
    let txt = match classify_float x with
      | FP_nan -> "NaN"
      | FP_infinite -> "NaN"
      | _ -> Printf.sprintf "%0.16g" x in
    value (box "double" (pcdata txt))

  let int n = value (box "i4" (pcdata (Int32.to_string n)))

  let methodCall name params =
    box "methodCall"
      (box "methodName" (pcdata name);
       box "params" (fun o -> List.iter (fun param -> box "param" param o) params))

  let string = function
      "" -> box "value" nothing
    | string -> value (pcdata string)

  let structure fields =
    value (box "struct" (fun o -> List.iter (fun (k, f) -> box "member" (fun o -> name k o; f o) o) fields))

  let fault n s =
    let faultCode = box "member" (fun o -> name "faultCode" o; int n o) in
    let faultString = box "member" (fun o -> name "faultString" o; string s o) in
    box "fault" (box "struct" (fun o -> faultCode o; faultString o))

  let success f =
    structure [
		"Status", string "Success";
		"Value", f
	]

  let error code params =
    structure [
		"Status", string "Failure";
		"ErrorDescription", (array (fun o -> List.iter (fun x -> string x o) (code :: params)));
	]

  let methodResponse response o =
    box "methodResponse"
      (fun o -> match response with
      | Success f -> box "params" (box "param" (success f)) o
      | Failure(code, params) -> box "params" (box "param" (error code params)) o
      | Fault(n, s) ->
		  box "fault" (structure ["faultCode", int n;
								  "faultString", string s]) o
      | Raw f ->
		  box "params" (box "param" f) o
	  ) o
end

module From = struct
  let id x = x

  let pcdata f = function
    | Xml.PCData string -> f string
    | xml -> rtte "pcdata" xml

  let unbox ok f = function
    | Xml.Element(s, [], data) when List.mem s ok -> f data
    | xml -> rtte (Printf.sprintf "unbox: %s should contain '%s'" (pretty_print xml) (List.hd ok)) xml

  let singleton ok f xml =
    unbox ok (function [x] -> f x | y -> rtte (Printf.sprintf "singleton: {%s} should be the singleton {%s}" (String.concat ", " (List.map pretty_print y)) (List.hd ok)) xml) xml

  let pair ok f1 f2 xml =
    unbox ok (function [v1; v2] -> f1 v1, f2 v2 | _ -> rtte "pair " xml) xml

  let value f xml = singleton ["value"] f xml

  (* <name> is only ever used inside a <struct><member>
     CA-20001: it is possible for <name> to be blank *)
  let name f xml = unbox ["name"]
    (function
     | [ Xml.PCData string ] -> f string
     | [ ] ->
	 f ""
	 | x -> rtte "From.name: should contain PCData" xml
    ) xml

  let check expected xml got =
    if got <> expected then rtte ("check " ^ expected) xml

  let nil = value (unbox ["nil"] (fun _ -> ()))

  let array f = value (singleton ["array"] (unbox ["data"] (List.map f)))

  let boolean = value (singleton ["boolean"] ((<>) (Xml.PCData "0")))

  let datetime x = Date.of_string (value (singleton ["dateTime.iso8601"] (pcdata id)) x)

  let double = value (singleton ["double"] (pcdata float_of_string))

  let int = value (singleton ["i4"; "int"] (pcdata Int32.of_string))

  let methodCall xml =
    pair ["methodCall"]
      (singleton ["methodName"] (pcdata id))
      (unbox ["params"] (List.map (singleton ["param"] id)))
    xml

  let string = function
    | Xml.Element("value", [], [Xml.PCData s])                  -> s
    | Xml.Element("value", [], [Xml.Element("string", [], [])])
    | Xml.Element("value", [], [])                              -> ""
    | xml                                                       -> value (singleton ["string"] (pcdata id)) xml

  let structure : Xml.xml -> (string * Xml.xml) list =
    singleton ["value"] (unbox ["struct"] (List.map (pair ["member"] (name id) id)))

  let success =
    unbox ["params"] (List.map (singleton ["param"] id))

  let status xml =
    let bindings = structure xml in
    try match string (List.assoc "Status" bindings) with
    | "Success" -> Success [ List.assoc "Value" bindings ]
    | "Failure" -> begin
	match array id (List.assoc "ErrorDescription" bindings) with
	| [] -> rtte "Empty array of error strings" (Xml.PCData "")
	| code::strings ->
	    Failure (string code, List.map string strings)
      end
    | _ -> raise Not_found
    with Not_found -> rtte "Status" xml

  let fault f =
    let aux m =
      int (List.assoc "faultCode" m), string (List.assoc "faultString" m) in
    singleton ["fault"] (fun xml -> aux (structure xml))

  let methodResponse xml =
    singleton ["methodResponse"]
      (function
       | Xml.Element("params", _, _) as xml -> begin match success xml with
       | [ xml ] -> status xml
       | _ -> rtte "Expected single return value (struct status)" xml
	 end
       | Xml.Element("fault", _, _) as xml ->
	   Fault (fault id xml)
       | xml -> rtte "response" xml)
   xml
end
