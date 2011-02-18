open Printf

(** A type of unique identifiers *)
type uid = int * string

let mk_uid =
  let ctr = ref 0 in
    fun (h:string) -> let uid = !ctr in ctr := !ctr + 1; (uid, h)

let string_of_uid (u,h) = "slot" ^ (string_of_int u) ^ "_" ^ h

(** Comparison binary operators. *)
type compop = Eq | Neq | Lt | Lte | Gt | Gte
  
(** Binary arithmetic operators. *)
type binop = 
    Plus | Times | Minus | Shl | Shr | Sar | And | Or
  | Xor | Compare of compop | Move
		   
(** Unary arithmetic operators. *)
type unop = Neg | Not | Lognot
    
(** Operands. *)
type operand =
  | Imm of int32 (* immediate int32 constant *)
  | Slot of uid  (* an abstract "storage location" *)

(** Simple Instructions *)
type insn =
  | BinArith of operand * binop * operand   (* a0 := a0 op a2 *)
  | UnArith of unop * operand               (* a0 := op a0 *)

(** Abstract labels -- just reuse the X86 labels *)
type lbl = X86.lbl

(** Control-flow operations *)
type cfinsn =
  | Ret of operand
  | Jump of lbl (* jump to a0 *)
  | If of operand * compop * operand * lbl * lbl
                (* if (a0 op a2) then jump a3 else jump a4 *)

(* type of basic blocks: labeled sequences terminated by a jump *)
type bb = {
    bb_lbl  : lbl;
    bb_body : insn list;
    bb_link : cfinsn;
  }

let mk_bb lbl insns link = {
    bb_lbl  = lbl;
    bb_body = insns;
    bb_link = link;
  }
  
  
type prog = {
  il_tmps:  uid list;   (* List of slot identifiers used in this program. *)
  il_cfg:   bb list;
  il_entry: lbl 
}



let string_of_compop = function | Eq -> "==" | Neq -> "!=" | Lt -> "<"
  | Lte -> "<=" | Gt -> ">" | Gte -> ">="
  
let string_of_binop = function 
  | Plus -> "+=" | Times -> "*=" | Minus -> "-="
  | Shl -> "<<=" | Shr -> ">>>=" | Sar -> ">>="
  | And -> "&=" | Or -> "|=" | Xor -> "^=" | Move -> ":="
  | Compare o -> (string_of_compop o) ^ "="
      
let string_of_unop = function | Neg -> "-" | Not -> "~" | Lognot -> "!"
  
let string_of_operand = function
  | Imm i -> Int32.to_string i
  | Slot u -> string_of_uid u
      
let string_of_insn = function
  | BinArith (a0,op,a1) -> sprintf "%s %s %s" (string_of_operand a0)
      (string_of_binop op) (string_of_operand a1)
  | UnArith (op,a0) -> sprintf "%s := %s%s" (string_of_operand a0)
      (string_of_unop op) (string_of_operand a0)

let string_of_cfinsn = function
  | Ret o -> sprintf "ret %s" (string_of_operand o)
  | Jump a -> sprintf "jump %s" (X86.string_of_lbl a)
  | If (a0,op,a2,l1,l2) -> sprintf "if (%s %s %s) then %s else %s"
      (string_of_operand a0) (string_of_compop op) (string_of_operand a2)
      (X86.string_of_lbl l1) (X86.string_of_lbl l2)

let string_of_bb b =
  sprintf "  block %s {\n    %s\n    %s\n  }"
      (X86.string_of_lbl b.bb_lbl)
      (String.concat "\n    " (List.map string_of_insn  b.bb_body))
      (string_of_cfinsn b.bb_link)

let string_of_prog {il_tmps=tmps; il_cfg=blocks; il_entry=l} =
  sprintf "prog[%s] @%s {\n%s\n}"
    (String.concat ", " (List.map string_of_uid tmps))
    (X86.string_of_lbl l)
    (String.concat "\n\n" (List.map string_of_bb blocks))

