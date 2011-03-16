open Ast
open Astlib
open Printf
open Tctxt

let report_error (info:Range.t) (expected:typ) (t:typ) : string =
  sprintf 
    "%s: This expression has type %s but an expression was expected of type %s." 
    (Range.string_of_range info) (string_of_typ expected) (string_of_typ t)

let rec tc_typ y z c : typ =
  if (tc_exp y c <> tc_exp z c) then
          failwith (report_error (exp_info y) (tc_exp y c) (tc_exp z c)) else TBool

and tc_bool y z c : typ = 
  if (tc_exp y c <> TBool) then
          failwith (report_error (exp_info y) TBool (tc_exp y c))
           else if (tc_exp z c <> TBool) then
          failwith (report_error (exp_info z) TBool (tc_exp z c))
          else TBool

and tc_int y z c : typ = 
  if (tc_exp y c <> TInt) then
          failwith (report_error (exp_info y) TInt (tc_exp y c))
         else if (tc_exp z c <> TInt) then
          failwith (report_error (exp_info z) TInt (tc_exp z c))
          else TInt
          
and tc_binop x y z (c:ctxt) : typ =
  begin match x with
        | Eq (_) -> tc_typ y z c
        | Neq (_) -> tc_typ y z c
        | And (_) -> tc_bool y z c
        | Or (_) -> tc_bool y z c
        | (_) -> tc_int y z c
  end

and tc_const x (c:ctxt) : typ =
  begin match x with
    | Cbool _ -> TBool
    | Cint _ -> TInt
    | Cstring _ -> TString
  end

and tc_unop x y c : typ =
  begin match x with
    | Neg (_) -> if (tc_exp y c <> TInt) then failwith "" else TInt
    | Not (_) -> if (tc_exp y c <> TInt) then failwith "" else TInt
    | Lognot (_) -> if (tc_exp y c <> TBool) then failwith "" else TBool
  end

and tc_lhs x c : typ =
  failwith "unimplemented"
  
and tc_new x y z c : typ =
  failwith "unimplemented"

and tc_ecall x y c : typ =
  failwith "unimplemented"
  

and tc_exp (e:Range.t exp) (c:ctxt) : typ =
 begin match e with
    | Binop (x,y,z) -> tc_binop x y z c
    | Const (x) -> tc_const x c
    | Lhs (x) -> tc_lhs x c
    | New (x,y,z) -> tc_new x y z c
    | Unop (x,y) -> tc_unop x y c
    | Ecall (x,y) -> tc_ecall x y c
  end

and tc_fdecl f c : unit =
  begin match f with
    | (rtyp, id, args, block, exp) -> ()
  end

and tc_vdecl v c: unit =
  begin match v with
    | {v_ty = x; v_id = y; v_init = z;} -> ()
  end

let get_decls (p:Range.t prog) : ctxt =
  let c = enter_scope empty_ctxt in
    let rec typecheck_h (c:ctxt) (h:Range.t Ast.gdecl) : ctxt =
      begin match h with
		      | Gvdecl v  ->
		        begin match v with
		          | {v_ty = x; v_id = y; v_init = z;} ->
		            begin match y with
		              | (_,a) -> add_vdecl a x c
		            end
		        end
	       | Gfdecl f ->
           begin match f with
		         | (rtyp, id, args, block, exp) ->
              begin match args with
                | h::tl -> 
                  begin match h with
                    | (_,a) -> 
                      begin match a with
                        | (_,x) -> add_fdecl x ([],rtyp) c
                      end
                  end
                | [] -> add_fdecl "" ([],rtyp) c
              end
		       end
		   end in
      List.fold_left typecheck_h c p 
  

let typecheck_prog (p:Range.t prog) : unit =
  let c = get_decls p in
    let rec typecheck_h (h:Range.t Ast.gdecl) : unit =
      begin match h with
		     | Gvdecl v  -> tc_vdecl v c
	       | Gfdecl f -> tc_fdecl f c
		   end in
      List.iter typecheck_h p