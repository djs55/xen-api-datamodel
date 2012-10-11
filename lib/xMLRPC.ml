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

  let pcdata f o = match Xmlm.input o with
	  | `Data x -> f x o
	  | _ -> failwith "pcdata"

  let unbox ok f o = match Xmlm.input o with
	  | `El_start (("", x), _) when List.mem x ok ->
		  let result = f o in
		  begin match Xmlm.input o with
		  | `El_end -> result
		  | _ -> failwith "unbox"
		  end
	  | _ -> failwith "unbox"

  let singleton ok f o =
    unbox ok (fun o ->
		match Xmlm.input o with
		| `El_start (("", x), _) ->
			begin match Xmlm.input o with
			| `El_end -> f o
			| _ -> failwith "singleton"
			end
		| _ -> failwith "singleton"
	) o

  let pair ok f1 f2 o =
    unbox ok (fun o ->
		let a = f1 o in
		let b = f2 o in
		a, b
	) o

  let value f o = singleton ["value"] f o

  let pcdata f o = match Xmlm.peek o with
	  | `Data x ->
		  ignore(Xmlm.input o);
		  f x
	  | `El_end | `El_start _ ->
		  f ""
	  | `Dtd _ -> failwith "pcdata"

  let string = singleton ["value" ]
	  (fun o -> match Xmlm.peek o with
	  | `Data x -> pcdata id o
	  | `El_start (("", "string"), []) -> singleton ["string"] (pcdata id) o
	  | `El_end -> ""
	  | _ -> failwith "string")

  let rec fold f acc o = match Xmlm.peek o with
	  | `El_end -> []
	  | `Dtd _ -> failwith "list"
	  | _ ->
		  let next = f acc o in
		  fold f next o


  let rec list f o = match Xmlm.peek o with
	  | `El_end -> []
	  | `Dtd _ -> failwith "list"
	  | _ ->
		  let first = f o in
		  first :: (list f o)

  (* <name> is only ever used inside a <struct><member>
     CA-20001: it is possible for <name> to be blank *)
  let name f o = unbox ["name"] (fun o -> pcdata f o) o

  let check expected got =
    if got <> expected then failwith (Printf.sprintf "expected %s got %s" expected got)

  let nothing o = ()

  let nil = value (unbox ["nil"] nothing)

  let array f = value (singleton ["array"] (unbox ["data"] f))

  let boolean = value (singleton ["boolean"] (fun o -> pcdata ((<>) "0") o))

  let datetime x = value (singleton ["dateTime.iso8601"] (pcdata id)) x

  let double = value (singleton ["double"] (pcdata float_of_string))

  let int = value (singleton ["i4"; "int"] (pcdata Int32.of_string))

  let methodCall f o =
	  pair ["methodCall"]
		  (singleton ["methodName"] (pcdata id))
		  (unbox ["params"] (list (singleton ["param"] f)))
		  o

  let structure f o =
	  singleton ["value"]
		  (unbox ["struct"]
			   (list
					(unbox ["member"]
						 (fun o ->
							 let key = name id o in
							 f key o
						 )
					)
			   )
		  ) o

  let status f o =
	  singleton ["value"]
		  (unbox ["struct"]
			   (fun o ->
				   unbox ["member"]
					   (fun o ->
						   let key = name id o in
						   if key <> "Status" then failwith "Status";
						   ignore(string o)
					   ) o;
				   unbox ["member"]
					   (fun o ->
						   match name id o with
						   | "Value" ->
							   f o
						   | "ErrorDescription" ->
							   let code = string o in
							   let params = list string o in
							   failwith (String.concat ", " (code :: params))
					   ) o
			   )
		  ) o

  let methodResponse f o =
	  singleton ["methodResponse"]
		  (fun o ->
			  match Xmlm.peek o with
			  | `El_start (("", "params"), _) ->
				  unbox ["params"] (status f) o
			  | `El_start (("", "fault"), _) ->
				  unbox ["fault"]
					  (structure
						   (fun key o -> match key with
						   | "faultCode" ->
							   ignore(int o)
						   | "faultString" ->
							   ignore(string o)
						   )
					  ) o;
				  failwith "fault"
			  | _ -> failwith "fault"
		  ) o
end
