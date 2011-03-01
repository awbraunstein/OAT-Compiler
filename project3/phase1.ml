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

type elt =
  | I of Il.insn
  | J of Il.cfinsn
  | L of Il.lbl

type stream = elt list

let compile_unop uop =
  match uop with
  | Ast.Neg    -> Il.Neg
  | Ast.Lognot -> Il.Lognot
  | Ast.Not    -> Il.Not

type elt =
  | I of Il.insn
  | J of Il.cfinsn
  | L of Il.lbl

type stream = elt list

let rec compile_exp (e: exp) (c:ctxt) (s: stream) : stream * operand =
  begin match e with
    | Binop (x,y,z) -> compile_binop x
    | Unop (x,y) -> compile_unop x
    | Cint x -> temp = (lookup y c)
    | Id y -> temp = (lookup y c)
      begin match temp with
      | None -> failwith "Variable not declared"
      | Some x -> ()
      end
  end
  
let compile_stmt (s:stmt)(t:stream) (c:ctxt) : stream * ctxt =
  begin match s with
    | Ast.Assign(l, e) -> Il.mk_uid compile_exp e 
    | Ast.If(e, st ,sto) -> ss
    | Ast.While(e, s) -> ss
    | Ast.For(vdl, eo, sto, s) -> ss
    | Ast.Block b -> compile block
  end

let rec compile_vardecl (v: var_decl list) (c: ctxt) : ctxt =
  begin match v with
    | h::tl -> 
      begin match h with
        | (ty, id, init) -> alloc id c
      end
    | [] -> c
  end
  
let rec compile_block (b:block)(c:ctxt)(s:stream) : ctxt * stream =
  begin match b with
    | (x,y) -> compile_vardecl x c
  end

let compile_prog ((block,ret):Ast.prog) : Il.prog =
	begin match ret with
	  | Cint x -> [Imm x] @ il_cfg
	  | Unop (x,y) -> [Imm 0l] @ il_cfg
	  | Binop (x,y,z) -> [Imm 0l] @ il_cfg
	  | Id x -> [Imm 0l]
	end
  let il_tmps = [] in
    let il_cfg = [] in
      let il_entry = "" in
      [compile_exp ret] @ il_cfg
