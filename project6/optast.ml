open Ast
open Astlib
open Range

let prog_new = []

let rec fold_exp (e:Range.t exp) : (Range.t exp) =
  begin match e with
    | Binop(bop,Const (Cint (_,c1)), Const (Cint (_,c2))) ->
      begin match bop with
        | Plus _ -> 
          let c = Int32.add c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | Minus _ ->
          let c = Int32.sub c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | Times _ -> 
          let c = Int32.mul c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | Eq _ -> 
          let c = (c1 = c2) in 
          (Const (Cbool (Range.norange,c)))
        | Neq _ -> 
          let c = (c1 != c2) in 
          (Const (Cbool (Range.norange,c)))
        | Lt _ -> 
          let c = (c1 < c2) in 
          (Const (Cbool (Range.norange,c)))
        | Lte _ -> 
          let c = (c1 <= c2) in 
          (Const (Cbool (Range.norange,c)))
        | Gt _ -> 
          let c = (c1 > c2) in 
          (Const (Cbool (Range.norange,c)))
        | Gte _ -> 
          let c = (c1 >= c2) in 
          (Const (Cbool (Range.norange,c)))
        | And _ -> 
          let c = Int32.logand c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | Or _ -> 
          let c = Int32.logor c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | IAnd _ -> 
          let c = Int32.logand c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | IOr _ -> 
          let c = Int32.logor c1 c2 in 
          (Const (Cint (Range.norange,c)))
        | Shl _ -> 
          let c = Int32.shift_left c1 (Int32.to_int c2) in 
          (Const (Cint (Range.norange,c)))
        | Shr _ -> 
          let c = Int32.shift_right c1 (Int32.to_int c2) in 
          (Const (Cint (Range.norange,c)))
        | Sar _ -> 
          let c = Int32.shift_right_logical c1 (Int32.to_int c2) in 
          (Const (Cint (Range.norange,c)))
      end
    | Binop(bop,Const (Cint (_,c1)), e1) ->
       Binop(bop,Const (Cint (Range.norange,c1)), fold_exp e1)
    | Binop(bop, e1,Const (Cint (_,c1))) ->
      Binop(bop,fold_exp e1, Const (Cint (Range.norange,c1)))
    | Binop(bop, e1, e2) ->
      Binop(bop,fold_exp e1, fold_exp e2)
    | Unop (unop,Const(Cint(_,c1))) ->
      begin match unop with
        | Neg _ -> let c = (Int32.neg c1) in (Const (Cint (Range.norange,c)))
        | Lognot _ ->let c = (Int32.lognot c1) in (Const (Cint (Range.norange,c)))
        | Not _ ->let c = (Int32.neg c1) in (Const (Cint (Range.norange,c)))
      end
    | Const c -> Const c 
    | This t -> This t
    | New (e1, i, e2) -> New(fold_exp e1,i,fold_exp e2)
    | Ctor (cid, el) -> Ctor (cid, el)
    | LhsOrCall lhsc ->
      begin match lhsc with
        | Lhs lhs ->
          begin match lhs with
            | Var id -> LhsOrCall(Lhs(Var id))
            | Path p -> LhsOrCall(Lhs(Path p))
            | Index (lhs_or_call, exp) -> LhsOrCall(Lhs(Index(lhs_or_call,fold_exp exp)))
          end
        | Call call ->
          begin match call with
            | Func (id,el) ->LhsOrCall(Call(Func (id,el)))
            | SuperMethod (id, el) ->LhsOrCall(Call(SuperMethod (id, el)))
            | PathMethod (path, el) ->LhsOrCall(Call(PathMethod (path, el)))
          end
      end
  end

and fold_vdecl (v:Range.t Ast.vdecl) : (Range.t Ast.vdecl) =
  begin match v with
    | {v_ty = TInt; v_id = id; v_init = init;} ->
      begin match init with
        | Ast.Iexp e -> let i = fold_exp e in {v_ty = TInt; v_id = id; v_init = (Ast.Iexp i);}
        | _ -> v
      end
    | _ -> v
  end
  
and vdecls_aux (vdecls : Range.t Ast.vdecls)(vbuff : Range.t Ast.vdecls): Range.t Ast.vdecls =
  begin match vdecls with
    | h::tl -> vdecls_aux tl (vbuff@[fold_vdecl h])
    | [] -> vbuff
  end 
  
and fold_vdecls (v:Range.t Ast.vdecls) : (Range.t Ast.vdecls) =
  vdecls_aux v []
  
and fdecls_aux (fdecls : Range.t Ast.fdecls)(fbuff : Range.t Ast.fdecls): Range.t Ast.fdecls =
  begin match fdecls with
    | h::tl -> fdecls_aux tl (fbuff@[fold_fdecl h])
    | [] -> fbuff
  end 
  
and fold_fdecls (v:Range.t Ast.fdecls) : (Range.t Ast.fdecls) =
  fdecls_aux v []
  
and stmt_aux stmts stmtbuff : Range.t Ast.stmt list =
  begin match stmts with
    | h::tl -> stmt_aux tl (stmtbuff@[fold_stmt h])
    | [] -> stmtbuff
  end 

and fold_block(block : Range.t Ast.block) : Range.t Ast.block =
  let (v,stmts) = block in 
  let n = fold_vdecls v in
  (n, stmt_aux stmts [])
  


and fold_stmt (stmt : Range.t Ast.stmt) : Range.t Ast.stmt =
  begin match stmt with
    | Assign (l,e) -> Assign(l,fold_exp e)
    | Scall (c) -> Scall (c)
    | Fail (e) -> Fail(fold_exp e)
    | If (e, s, os) -> 
      begin match os with
        | Some x -> If(fold_exp e, fold_stmt s, Some (fold_stmt x))
        | None -> If(fold_exp e, fold_stmt s, os)
      end
    | IfNull (ref, id, exp, stmt, os) -> 
      begin match os with
        | Some x -> IfNull(ref, id, fold_exp exp, fold_stmt stmt, Some(fold_stmt x))
        | None -> IfNull(ref, id, fold_exp exp, fold_stmt stmt, os)
      end
    | Cast (cid,id, exp, stmt, os) -> 
      begin match os with
        | Some x -> Cast (cid,id, fold_exp exp, fold_stmt stmt, Some(fold_stmt x))
        | None -> Cast (cid,id, fold_exp exp, fold_stmt stmt, os)
      end
    | While (exp, stmt) -> While(fold_exp exp, fold_stmt stmt)
    | For (vdecls, opt_exp, opt_stmt, stmt) -> For(vdecls, opt_exp, opt_stmt, fold_stmt stmt)
    | Block (block) -> Block(fold_block block)
  end

and fold_cdecl ((a,b,f,c,fdecls):Range.t Ast.cdecl) : Range.t Ast.cdecl =
  (a,b,f,c,fold_fdecls fdecls)


and fold_fdecl ((a, (b,c), d, block, e):Range.t Ast.fdecl) : Range.t Ast.fdecl =
  begin match e with
    | Some exp -> (a,(b,c), d, fold_block block, Some (fold_exp exp))
    | None ->  (a,(b,c), d, fold_block block, e)
  end
  

let rec parse_prog (prog:Range.t Ast.prog)(new_prog:Range.t Ast.prog):(Range.t Ast.prog) =  
  begin match prog with
    | h::tl ->
      begin match h with
        | Ast.Gvdecl vdecl-> parse_prog tl (new_prog@[Ast.Gvdecl(fold_vdecl vdecl)])
        | Ast.Gfdecl fdecl-> parse_prog tl (new_prog@[Ast.Gfdecl(fold_fdecl fdecl)])
        | Ast.Gefdecl efdecl -> parse_prog tl (new_prog@[Ast.Gefdecl efdecl])
        | Ast.Gcdecl cdecl -> parse_prog tl (new_prog@[Ast.Gcdecl cdecl])
      end
    | [] -> new_prog
  end
  
let opt_ast (prog:Range.t Ast.prog) : (Range.t Ast.prog) = 
  print_prog prog; let p = parse_prog prog prog_new in print_prog p; p