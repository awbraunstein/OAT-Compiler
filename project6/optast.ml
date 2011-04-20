open Ast
open Astlib
open Range

let prog_new = []

let fold_exp (e:Range.t exp) : (Range.t exp) =
  begin match e with
    (*| Binop(bop,e1,e2) ->
      begin match bop with
        | Ast.Plus _ -> e
        | _ -> e
      end*)
    | Binop(bop,Const (Cint (_,c1)), Const (Cint (_,c2))) ->
      let c = Int32.add c1 c2 in (Const (Cint (Range.norange,c)))
    | Unop (unop,e1) -> e
    | _ -> e
  end

let fold_vdecl (v:Range.t Ast.vdecl) : (Range.t Ast.vdecl) =
  begin match v with
    | {v_ty = TInt; v_id = _; v_init = init;} ->
      begin match init with
        | Ast.Iexp e -> v
        | _ -> v
      end
    | _ -> v
  end

let fold_block(block : Range.t Ast.block) : (Range.t Ast.block) =
  block


let rec fold_stmt (stmt : Range.t Ast.stmt) : (Range.t Ast.stmt) =
  begin match stmt with
    | Assign (l,e) -> Assign(l,fold_exp e)
    | Scall (c) -> stmt
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


let fold_fdecl ((a, (b,c), d, block, e):Range.t Ast.fdecl) : Range.t Ast.fdecl =
  (a,(b,c), d, fold_block block, e)
  

let rec parse_prog (prog:Range.t Ast.prog)(new_prog:Range.t Ast.prog):(Range.t Ast.prog) =  
  begin match prog with
    | h::tl ->
      begin match h with
        | Ast.Gvdecl vdecl-> parse_prog tl (new_prog@[Ast.Gvdecl(fold_vdecl vdecl)])
        | Ast.Gfdecl fdecl-> parse_prog tl (new_prog@[Ast.Gfdecl(fold_fdecl fdecl)])
        | _ -> parse_prog tl new_prog
      end; 
    | [] -> new_prog
  end
  
let opt_ast (prog:Range.t Ast.prog) : (Range.t Ast.prog) = 
  parse_prog prog prog_new