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
open Datamodel_types

(* For values that can be easily represented as strings, return the string *)
exception Map_key_that_cannot_be_represented_as_string
let to_string v =
  match v with
    VString s -> s
  | VInt i -> Int64.to_string i
  | VFloat f -> string_of_float f
  | VEnum e -> e
  | _ -> raise Map_key_that_cannot_be_represented_as_string      
      
let rec to_xml v =
  match v with
    VString s -> XMLRPC.To.string s
  | VInt i -> XMLRPC.To.string (Int64.to_string i)
  | VFloat f -> XMLRPC.To.double f
  | VBool b -> XMLRPC.To.boolean b
  | VDateTime d -> XMLRPC.To.datetime d
  | VEnum e -> XMLRPC.To.string e
  | VMap vvl -> XMLRPC.To.structure (List.map (fun (v1,v2)-> to_string v1, to_xml v2) vvl)
  | VSet vl -> XMLRPC.To.array (List.map (fun v->to_xml v) vl)
  | VRef r -> XMLRPC.To.string r
      
