
open Ast
open Astlib
let prog_new = []

let fold_vdecl (v:Range.t Ast.vdecl) : (Range.t Ast.vdecl) =
  begin match v with
    | {v_ty = TInt; v_id = _; v_init = init;} ->
      begin match init with
        | Ast.Iexp e ->
          begin match e with
            | Binop(bop,e1,e2) -> v
            | _ -> v
          end
        | _ -> v
      end
    | _ -> v
  end

let fold_block(block : Range.t Ast.block) : Range.t Ast.block =
  block


let fold_stmt (stmt : Range.t Ast.stmt) : Range.t Ast.stmt =
  begin match stmt with
    | Assign (l,e) -> stmt
    | Scall (c) -> stmt
    | Fail (e) -> stmt
    | If (e, s, os) -> stmt
    | IfNull (ref, id, exp, stmt, opt_stmt) -> stmt
    | Cast (cid,id, exp, stmt, opt_stmt) -> stmt
    | While (exp, stmt) -> stmt
    | For (vdecls, opt_exp, opt_stmt, stmt) -> stmt
    | Block (block) -> stmt
  end


let fold_fdecl ((a, (b,c), d, block, e):Range.t Ast.fdecl) : Range.t Ast.fdecl =
  (a,(b,c), d, fold_block block, e)
  

let rec parse_prog (prog:Range.t Ast.prog)(new_prog:Range.t Ast.prog):(Range.t Ast.prog) =  
  begin match prog with
    | h::tl ->
      begin match h with
        | Ast.Gvdecl vdecl-> new_prog
        | Ast.Gfdecl fdecl-> new_prog
        | _ -> parse_prog tl new_prog
      end; 
    | [] -> new_prog
  end
  
let opt_ast (prog:Range.t Ast.prog) : (Range.t Ast.prog) = 
  parse_prog prog prog_new