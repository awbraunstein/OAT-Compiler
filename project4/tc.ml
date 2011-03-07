open Ast
open Astlib
open Printf
open Tctxt

let report_error (info:Range.t) (expected:typ) (t:typ) : string =
  sprintf 
    "%s: This expression has type %s but an expression was expected of type %s." 
    (Range.string_of_range info) (string_of_typ expected) (string_of_typ t)



let typecheck_prog (p:Range.t prog) : unit =
failwith "unimplemented"
