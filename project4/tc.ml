open Ast
open Astlib
open Printf
open Tctxt

let report_error (info:Range.t) (expected:typ) (t:typ) : string =
  sprintf 
    "%s: This expression has type %s but an expression was expected of type %s." 
    (Range.string_of_range info) (string_of_typ expected) (string_of_typ t)



let typecheck_prog (p:Range.t prog) : unit =
  begin match p with
    | h::tl ->
      begin match h with
        | _  -> failwith "error"
      end
    | _ -> failwith "no program"
  end