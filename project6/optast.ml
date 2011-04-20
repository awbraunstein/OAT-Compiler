let prog_new = []

let rec parse_prog (prog:Range.t Ast.prog): unit =  
  begin match prog with
    | h::tl ->
      begin match h with
        | Ast.Gvdecl vdecl-> ()
        | Ast.Gfdecl fdecl-> ()
        | _ -> parse_prog tl
      end; 
    | [] -> ()
  end
  
let opt_ast (prog:Range.t Ast.prog) : (Range.t Ast.prog) = 
  parse_prog prog; prog_new
  

