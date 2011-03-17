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
    | Var (_,s) -> let a = lookup_vdecl s c in
      begin match a with
        | Some t -> t
        | None -> failwith "not declared"
      end
    | Index (a,b) -> tc_exp b c
  end
  
and tc_new e1 id e2 c : typ =
  if (tc_exp e1 c <> TInt) then
    failwith (report_error (exp_info e2) (TInt) (tc_exp e1 c)) else TArray (tc_exp e2 c)


and ecall_h y c : typ list=
  let l = [] in
  begin match y with
    | h:: tl -> tc_exp h c; ecall_h tl c; l
  end
  
and tc_ecall x y c : typ = 
  let f = lookup_fdecl x c in
  begin match f with
    | Some (l,r) ->List.iter2 (fun a -> fun b -> if a = tc_exp b c then ()
        else failwith "exception") l y;
        begin match r with
          | Some t -> t
          | None -> failwith "no type"
        end
    | None -> failwith "not declared yet"
  end
  

and tc_exp (e:Range.t exp) (c:ctxt) : typ =
 begin match e with
    | Binop (x,y,z) -> tc_binop x y z c
    | Const (x) -> tc_const x c
    | Lhs (x) -> tc_lhs x c
    | New (x,y,z) -> tc_new x y z c
    | Unop (x,y) -> tc_unop x y c
    | Ecall ((_,id),y) ->tc_ecall id y c
  end

  
and tc_stmt (s: Range.t stmt) (c:ctxt) : unit  =
  begin match s with
    | Assign (l,e) -> ignore (tc_exp e c)
    | Scall (i,e) -> 
      begin match e with
        | h::tl -> ignore(tc_exp h c)
        | [] -> failwith "!"
      end
    | If (e,st,Some o) -> ignore (tc_exp e c); tc_stmt st c; tc_stmt o c
    | While (e,s) -> ignore(tc_exp e c); tc_stmt s c;
    | For (v,Some oe,Some os,st) -> tc_stmt st c; ignore(tc_exp oe c); tc_stmt os c
    | Block b -> tc_block b c
    | _ -> failwith "empty statement"
  end
  

and vdecl_h (v:Range.t Ast.vdecl list) (c:ctxt) =
  begin match v with
    | h::tl -> tc_vdecl h c; vdecl_h tl c
    | _ -> ()
  end

and stmt_h (s:Range.t stmt list) (c:ctxt) : unit =
  begin match s with
    | h::tl -> tc_stmt h c; stmt_h tl c
    | _ -> ()
  end

and tc_block (b:Range.t block) (c:ctxt) : unit =
  begin match b with
    | (x,y) -> vdecl_h x c; stmt_h y c
  end
  
and tc_fdecl (f:Range.t fdecl) (c:ctxt) : unit  =
  begin match f with
    | (rtyp, id, args, block, exp) ->
      let c = List.fold_left (fun c -> (fun (t,(_,id)) -> add_vdecl id t c)) c args in
        tc_block block c;
      begin match exp with
        | Some e -> ignore(tc_exp e c)
        | None -> failwith "erorr"
      end
  end

and tc_init (i:Range.t init) (c:ctxt) : typ =
   begin match i with
        | Iexp e -> tc_exp e c
        | Iarray (_,l) ->
          begin match l with
            | h::tl -> let typ1 = tc_init h c in
              List.iter (fun init -> if (tc_init init c = typ1) then ()
                else failwith "wrong types") tl; TArray typ1
            | [] -> failwith "must have length greater than 1"
          end
      end
      
and tc_vdecl (v:Range.t vdecl) (c:ctxt) : unit =
  begin match v with
    | {v_ty = ty; v_id = id; v_init = i;} ->
       if (tc_init i c <> ty) then
          failwith (report_error (init_info i) (ty) (tc_init i c)) else ()
  end

let get_decls (p: Range.t prog) : ctxt =
  let c = enter_scope empty_ctxt in
  let rec typecheck_h (c: ctxt) (h: Range.t Ast.gdecl) : ctxt =
    begin match h with
      | Gvdecl { v_ty = x; v_id = y; v_init = z;} ->
        begin match y with
          | (_, a) -> add_vdecl a x c
        end
      | Gfdecl (rtyp, (_, a), args, block, exp) ->
        let (f, _) = List.split args in
          add_fdecl a (f, rtyp) c
    end in  List.fold_left typecheck_h c p

let typecheck_prog (p:Range.t prog) : unit =
  let c = get_decls p in
    let rec typecheck_h (h:Range.t Ast.gdecl) : unit =
      begin match h with
		     | Gvdecl v  -> tc_vdecl v c
	       | Gfdecl f -> tc_fdecl f c
		   end in
      List.iter typecheck_h p