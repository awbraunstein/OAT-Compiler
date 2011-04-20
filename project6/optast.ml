let prog_new = []


let fold_fdecl ((_, (_,fid), args, block, reto):Range.t Ast.fdecl) : Range.t Ast.fdecl =
  
  



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
  

