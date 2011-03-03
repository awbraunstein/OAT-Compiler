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

let compile_lhs (l:lhs) : string = 
  begin match l with
    | Var x -> x
  end
  
type elt =
  | I of Il.insn
  | J of Il.cfinsn
  | L of Il.lbl

type stream = elt list
(*
let rec compile_vardecl (v: var_decl list) (c: ctxt) (s:stream) : stream*ctxt =
  (c,s)
  begin match v with
    | h::tl -> 
      begin match h with
        | (ty, id, init) -> alloc id c
      end
    | [] -> c
  end
*)
let rec compile_exp (e: exp) (c:ctxt) (s: stream) : stream * operand * ctxt=
  begin match e with
    | Binop (x,y,z) -> 
      begin match compile_exp y c [] with
        | (stream1, operand1, ctxt1) -> 
          begin match compile_exp z ctxt1 [] with
            | (stream2, operand2, ctxt2) ->
              (s@stream1@stream2@[I(BinArith(operand1, compile_binop x, operand2))], operand1, ctxt2)
          end
      end      
    | Unop (x,y) ->
    begin match compile_exp y c [] with
        | (stream1, operand1, ctxt1) -> 
          (s@stream1@[I(UnArith(compile_unop x, operand1))], operand1, ctxt1)
      end      
    | Cint x ->(s,Imm x, c)
    | Id y ->
      begin match lookup y c with
	      | None -> failwith "Variable not in scope"
	      | Some u -> (s, Slot u, c)
      end
  end
    

let rec compile_stmt (stm:stmt)(t:stream) (c:ctxt) : stream*ctxt =
  begin match stm with
    | Ast.Assign(l, e) ->
      begin match compile_exp e c [] with
        | (stream1, operand1, ctxt1) -> 
          begin match lookup (compile_lhs l) ctxt1 with
	          | None -> failwith "Variable not in scope"
		        | Some u ->
              (t@stream1@[I(BinArith(Slot u, Il.Move, operand1))],ctxt1)
          end
      end     
    (*| Ast.If(e, st ,sto) -> *)
    | Ast.While(e, s) ->
      let __lpre =  X86.mk_lbl() in 
      let __lbody = X86.mk_lbl() in
      let __lpost = X86.mk_lbl() in
        begin match compile_exp e c t with
          | (new_stream, op, new_ctxt) -> 
            let ctxt_step = enter_scope new_ctxt in
	            begin match compile_stmt stm new_stream ctxt_step with
	              | (str3, ctxt3) -> (t@[L(__lpre)]@new_stream@
	                [J(Il.If(op, Eq, Imm 1l, __lbody, __lpost))]@
	                [L(__lbody)]@str3@[L(__lpost)], leave_scope ctxt3)
	            end
        end
    (*| Ast.For(vdl, eo, sto, s) -> ss*)
    | Ast.Block b ->
      begin match compile_block b (enter_scope c) [] with
        | (stream_new, ctxt_new) -> (t@stream_new, leave_scope ctxt_new)
      end
  end
    

 (*TOPLEVEL*)

and compile_block(b:block)(c:ctxt)(s:stream) : stream*ctxt =
  (s,c)
  (*
  begin match b with
    | (x,y) -> 
      begin match compile_vardecl x c s with
        | (c2, s2) -> 
          let c1 = c2;
          let s1 = s2;
          (c1, s1);
          compile_stmt y s1 c2
      end              
  end
 *) 
let compile_prog ((block,ret):Ast.prog) : Il.prog =
failwith "unimplemented"
      
