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



let compile_prog ((block,ret):Ast.prog) : Il.prog =
  begin match ret with
    | Cint x -> Imm x
    | Unop (x,y) -> compile_unop ret
    | Binop (x,y,z) -> compile_binop ret
    | Id x -> Imm 3l
  end