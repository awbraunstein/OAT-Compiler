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
          failwith (report_error (exp_info z) (tc_exp y c) (tc_exp z c)) else TBool

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
    | Neg (_) -> if (tc_exp y c <> TInt) then
      failwith (report_error (exp_info y) TInt (tc_exp y c)) else TInt
    | Not (_) -> if (tc_exp y c <> TInt) then
      failwith (report_error (exp_info y) TInt (tc_exp y c)) else TInt
    | Lognot (_) -> if (tc_exp y c <> TBool) then
      failwith (report_error (exp_info y) TBool (tc_exp y c)) else TBool
  end

and tc_lhs x c : typ =
  begin match x with
    | Var a -> TInt
    | Index (a,b) -> TInt
  end
  
and tc_new x y z c : typ =
  if (tc_exp x c <> tc_exp z c) then
    failwith (report_error (exp_info z) (tc_exp x c) (tc_exp z c)) else (tc_exp x c)

and tc_ecall x y c : typ = 
  begin match x with 
    | (_,_) -> TInt
  end
  

and tc_exp (e:Range.t exp) (c:ctxt) : typ =
 begin match e with
    | Binop (x,y,z) -> tc_binop x y z c
    | Const (x) -> tc_const x c
    | Lhs (x) -> tc_lhs x c
    | New (x,y,z) -> tc_new x y z c
    | Unop (x,y) -> tc_unop x y c
    | Ecall (x,y) -> tc_ecall x y c
  end

  
and tc_stmt (s: Range.t stmt) (c:ctxt)  =
  begin match s with
    | Assign (l,e) -> tc_exp e c
    | Scall (i,e) -> 
      begin match e with
        | h::tl -> tc_exp h c
      end
    | If (e,s,o) -> tc_exp e c; tc_stmt s c;
      begin match o with
        | Some st -> tc_stmt st c
        | None -> failwith "!"
      end
    | While (e,s) -> tc_stmt s c; tc_exp e c
    | For (v,oe,os,s) -> tc_stmt s c;
      begin match oe with
        | Some e -> tc_exp e c
        | None -> failwith "!"
      end;
      begin match os with
        | Some st -> tc_stmt st c
        | None -> failwith "!"
      end;
    | Block b ->failwith "" 
  end
  
and tc_rtyp (r:rtyp) (c:ctxt) : unit =
  begin match r with
    | Some t -> ()
    | None -> ()
  end

and tc_id (i: Range.t id) (c:ctxt) : unit =
  ()

and tc_args (a:Range.t args) (c:ctxt) : unit =
  ()

and vdecl_h (v:Range.t Ast.vdecl list) (c:ctxt) =
  begin match v with
    | h::tl -> tc_vdecl h c; vdecl_h tl c
    | _ -> ()
  end

and stmt_h (s:Range.t stmt list) (c:ctxt) : unit =
  begin match s with
    | h::tl -> ignore (tc_stmt h c); stmt_h tl c
    | _ -> ()
  end

and tc_block (b:Range.t block) (c:ctxt) : unit =
  begin match b with
    | (x,y) -> vdecl_h x c; stmt_h y c
  end
  
and tc_fdecl (f:Range.t fdecl) (c:ctxt) : unit =
  begin match f with
    | (rtyp, id, args, block, exp) ->
      ignore (tc_rtyp rtyp c);
      ignore (tc_id id c);
      ignore (tc_args args c);
      ignore (tc_block block c);
      begin match exp with
        | Some e -> ignore (tc_exp e c); ()
        | None -> failwith "wtf"
      end
  end

and tc_vdecl (v:Range.t vdecl) (c:ctxt) : unit =
  begin match v with
    | {v_ty = x; v_id = y; v_init = z;} -> ()
  end

let rec get_args (l:Range.t Ast.args) : typ list =
  let r = [] in 
  begin match l with
    | h::tl -> 
      begin match h with
       | (t,_) -> r@[t]@(get_args tl)
      end
    | _ -> r@[]
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
              let f = get_args args in
                begin match id with
                  | (_,a) -> add_fdecl a (f,rtyp) c
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