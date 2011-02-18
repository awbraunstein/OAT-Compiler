(** A type of unique identifiers *)
type uid = int * string

val mk_uid : string -> uid
val string_of_uid : uid -> string

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

val mk_bb : lbl -> (insn list) -> cfinsn -> bb
  
type prog = {
  il_tmps:  uid list;   (* List of slot identifiers used in this program. *)
  il_cfg:   bb list;
  il_entry: lbl 
}



val string_of_compop : compop -> string 
  
val string_of_binop : binop -> string 
      
val string_of_unop : unop -> string
  
val string_of_operand : operand -> string

val string_of_insn : insn -> string

val string_of_cfinsn : cfinsn -> string

val string_of_bb : bb -> string

val string_of_prog : prog -> string
