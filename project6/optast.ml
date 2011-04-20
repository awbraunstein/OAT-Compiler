
let opt_ast (prog:Range.t Ast.prog) : (Range.t Ast.prog) = 
  begin match prog with
    | h::tl ->
      begin match h with
        | Ast.Gvdecl vdecl-> prog
        | _ -> prog
      end
    | [] -> prog
  end
