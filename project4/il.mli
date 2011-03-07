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
  | Imm of int32                (* immediate int32 constant *)
  | Global of Cunit.global_data (* a global variable *)
  | Arg of int                  (* an argument *)
  | Slot of uid                 (* an abstract "storage location" *)

(** Abstract labels -- just reuse the X86 labels *)
type lbl = X86.lbl

(** Simple Instructions *)
type insn =
  | BinArith of operand * binop * operand           (* a0 := a0 op a2 *)
  | UnArith of unop * operand                       (* a0 := op a0 *)
  | Alloc of operand * operand                      (* a0 := alloc a1 *) 
  | AddrOf of operand * operand * operand           (* a0 := &(a1[a2]) *)
  | Load of operand * operand                       (* a0 := *a1 *)
  | Store of operand * operand                      (* *a0 := a1 *)
  | Call of operand option * string * operand list  (* a0 := call f a2 *)

(** Control-flow operations *)
type cfinsn =
  | Ret of operand option
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
  
type fdecl = {
  il_name: string;      (* The name of this function. *)
  il_num_of_args: int;  (* For debugging: the number of arguments. *)
  il_tmps:  uid list;   (* List of slot identifiers used in this function. *)
  il_entry:  lbl;       (* The entry label of this function. *)
  il_cfg:   bb list;    
}  
  
type prog = {
  il_globals: Cunit.global_data list;
  il_fdecls: fdecl list;
  il_start: string 
}



val string_of_compop : compop -> string 
  
val string_of_binop : binop -> string 
      
val string_of_unop : unop -> string
  
val string_of_operand : operand -> string

val string_of_insn : insn -> string

val string_of_cfinsn : cfinsn -> string

val string_of_bb : bb -> string

val string_of_prog : prog -> string

val c_string_of_prog : prog -> string

val c_run_prog : prog -> string -> string
