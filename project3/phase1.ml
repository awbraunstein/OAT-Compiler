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
and compile_vardecl (v: var_decl list) (c: ctxt) (s:stream) : stream * ctxt =
  let rec compile_decl(v: var_decl list)(c: ctxt)(s:stream) :stream * ctxt = 
	  begin match v with
	    | [] -> (s,c)
      | h::tl -> 
        begin match alloc h.v_id c with
          | (ctxt_new, uid_new) ->  
            begin match compile_stmts [Ast.Assign(Var(h.v_id), h.v_init)] [] ctxt_new with
              | (stream_ass, ctxt_ass) -> compile_decl tl ctxt_ass (s@stream_ass)
            end
        end
    end in compile_decl v c s
    
and compile_stmts(sl:stmt list)(t:stream)(c:ctxt): stream*ctxt =    
	let rec compile_stmt (sl:stmt list)(t:stream) (c:ctxt) : stream*ctxt =
    begin match sl with
      | [] -> (t,c)
      | h::tl -> 
			  begin match h with
			    | Ast.Assign(l, e) ->
			      begin match compile_exp e c [] with
			        | (stream1, operand1, ctxt1) -> 
			          begin match lookup (compile_lhs l) ctxt1 with
				          | None -> failwith "Variable not in scope"
					        | Some u ->
			              compile_stmt tl (t@stream1@[I(BinArith(Slot u, Il.Move, operand1))]) ctxt1
			          end
			      end     
			    | Ast.If(e, s ,sto) -> 
			      let __lpre =  X86.mk_lbl() in 
			      let __lbody = X86.mk_lbl() in
			      let __lelse = X86.mk_lbl() in
			      let __lpost = X86.mk_lbl() in
			        begin match compile_exp e c t with
			          | (new_stream, op, new_ctxt) -> 
			            begin match compile_stmt [s] [] (enter_scope new_ctxt) with
			              | (str3, ctxt3) -> 
			                begin match sto with
			                  | None -> compile_stmt tl (t@
			                    [L(__lpre)]@
			                    new_stream@
			                    [J(Il.If(op, Neq, Imm 0l, __lbody, __lpost))]@
			                    [L(__lbody)]@str3@[L(__lpost)]) (leave_scope ctxt3)
			                  | Some x -> 
			                    begin match compile_stmt [x] [] (enter_scope new_ctxt) with
			                      | (str_else, ctxt_else) -> compile_stmt tl (t@
			                        [L(__lpre)]@
			                        new_stream@
			                        [J(Il.If(op, Il.Neq, Imm 0l, __lbody, __lelse))]@
			                        [L(__lbody)]@str3@[J(Il.Jump __lpost)]@
			                        [L(__lelse)]@str_else@
			                        [L(__lpost)]) (leave_scope ctxt_else)
			                    end
			                end
			            end
			        end
			    | Ast.While(e, s) ->
			      let __lpre =  X86.mk_lbl() in 
			      let __lbody = X86.mk_lbl() in
			      let __lpost = X86.mk_lbl() in
			        begin match compile_exp e c t with
			          | (new_stream, op, new_ctxt) -> 
			            begin match compile_stmt [s] new_stream (enter_scope new_ctxt) with
			              | (str3, ctxt3) -> compile_stmt tl (t@[L(__lpre)]@new_stream@
			                [J(Il.If(op, Neq, Imm 0l, __lbody, __lpost))]@
			                [L(__lbody)]@str3@[J(Il.Jump __lpre)]@
			                [L(__lpost)]) (leave_scope ctxt3)
			            end
			        end
			   (* | Ast.For(vdl, eo, sto, s) -> 
			      begin match compile_vardecl vdl c [] with
			        | (stream1, ctxt1) -> 
			          begin match *)
			    | Ast.Block b ->
			      begin match compile_block b (enter_scope c) [] with
			        | (stream_new, ctxt_new) -> compile_stmt tl (t@stream_new) (leave_scope ctxt_new)
			      end
        end
    end in compile_stmt sl t c
and compile_block(b:block)(c:ctxt)(s:stream) : stream*ctxt =
  begin match b with
    | (vd,sl) -> 
      begin match compile_vardecl vd c [] with
        | (s2, c2) -> compile_stmts sl s2 c2
      end              
  end

let compile_prog ((block,ret):Ast.prog) : Il.prog =
failwith "unimplemented"
      
