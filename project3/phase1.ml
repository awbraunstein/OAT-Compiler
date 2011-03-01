open Ast
open Il
open Ctxt
open Printf

(* Parse an AST from a preexisting lexbuf. 
 * The filename is used to generate error messages.
*)
let parse (filename : string) (buf : Lexing.lexbuf) : Ast.prog =
  try
    Lexer.reset_lexbuf filename buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwith (sprintf "Parse error at %s."
        (Range.string_of_range (Lexer.lex_range buf)))

(* Generate a fresh temporary name *)
let mk_tmp : unit -> string =
  let ctr = ref 0 in
    fun () -> let c = !ctr in ctr := !ctr + 1; "tmp" ^ (string_of_int c)

(* Some potentially useful helper functions: *)
let compile_binop bop = 
  match bop with
  | Ast.Plus -> Il.Plus
  | Ast.Times -> Il.Times
  | Ast.Minus -> Il.Minus
  | Ast.Eq -> Il.Compare Il.Eq
  | Ast.Neq -> Il.Compare Il.Neq
  | Ast.Lt -> Il.Compare Il.Lt
  | Ast.Lte -> Il.Compare Il.Lte
  | Ast.Gt -> Il.Compare Il.Gt
  | Ast.Gte -> Il.Compare Il.Gte
  | Ast.And -> Il.And
  | Ast.Or -> Il.Or
  | Ast.Shl -> Il.Shl
  | Ast.Shr -> Il.Shr
  | Ast.Sar -> Il.Sar

let compile_unop uop =
  match uop with
  | Ast.Neg    -> Il.Neg
  | Ast.Lognot -> Il.Lognot
  | Ast.Not    -> Il.Not

let rec compile_vardecl (v: var_decl list) =
  begin match v with
    | h::tl -> [Il.mk_uid h.v_id] @ compile_vardecl tl
    | [] -> []
  end


let rec compile_exp (e: exp) =
  begin match e with
    | []
  end
  
let rec compile_block (b: block) =
  begin match b with
    | []
  end

let compile_prog ((block,ret):Ast.prog) : Il.prog =
  let il_tmps = [] in
    let il_cfg = [] in
      let il_entry = "" in
        begin match block with
          | (x,y) -> compile_vardecl x @ il_tmps;
            begin match y with
              | []
            end
        end
        begin match ret with
          | Cint x -> [Imm x] @ il_cfg
          | Unop (x,y) -> [Imm 0l] @ il_cfg
          | Binop (x,y,z) -> [Imm 0l] @ il_cfg
          | Id x -> [Imm 0l]
        end