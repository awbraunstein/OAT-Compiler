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
  | Call of operand option * string * operand list  (* a0 := call f a1 *)

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


let mk_bb lbl insns link = {
    bb_lbl  = lbl;
    bb_body = insns;
    bb_link = link;
  }

type fdecl = {
  il_name: string;       (* The name of this function. *)
  il_num_of_args: int;  (* For debugging: the number of arguments. *)
  il_tmps:  uid list;   (* List of slot identifiers used in this function. *)
  il_entry:  lbl;       (* The entry label of this function. *)
  il_cfg:   bb list;    
}  
  
type prog = {
  il_globals: Cunit.global_data list;
  il_fdecls: fdecl list;
  il_start: string; 
}


let string_of_compop = function | Eq -> "==" | Neq -> "!=" | Lt -> "<"
  | Lte -> "<=" | Gt -> ">" | Gte -> ">="
  
let string_of_unop = function | Neg -> "-" | Not -> "~" | Lognot -> "!"
  
let string_of_operand = function
  | Imm i -> Int32.to_string i
  | Global {Cunit.label=l} -> X86.string_of_lbl l
  | Arg i -> "arg" ^ string_of_int i
  | Slot u -> string_of_uid u
      
let string_of_binop = function 
  | Plus -> "+=" | Times -> "*=" | Minus -> "-="
  | Shl -> "<<=" | Shr -> ">>>=" | Sar -> ">>="
  | And -> "&=" | Or -> "|=" | Xor -> "^=" | Move -> ":="
  | Compare o -> (string_of_compop o) ^ "="
      
let string_of_insn = function
  | BinArith (a0,op,a1) -> sprintf "%s %s %s" (string_of_operand a0)
      (string_of_binop op) (string_of_operand a1)
  | UnArith (op,a0) -> sprintf "%s := %s%s" (string_of_operand a0)
      (string_of_unop op) (string_of_operand a0)
  | Alloc (a0, a1) -> sprintf "%s := alloc %s" (string_of_operand a0)
      (string_of_operand a1)                   
  | AddrOf (a0, a1, a2) -> sprintf "%s := addrof %s %s" (string_of_operand a0)
      (string_of_operand a1) (string_of_operand a2)
  | Load (a0, a1) -> sprintf "%s := *%s (load)" (string_of_operand a0)
      (string_of_operand a1)                                      
  | Store (a0, a1) -> sprintf "*%s := %s (store)" (string_of_operand a0)
      (string_of_operand a1)                                      
  | Call (opa0, fid, as1) ->
      match opa0 with
        | None -> sprintf "call %s (%s)" 
           fid (String.concat ", " (List.map string_of_operand as1))
        | Some a0 -> sprintf "%s := call %s (%s)" (string_of_operand a0) 
           fid (String.concat ", " (List.map string_of_operand as1))

let string_of_cfinsn = function
  | Ret (Some o) -> sprintf "ret %s" (string_of_operand o)
  | Ret None -> sprintf "ret"
  | Jump a -> sprintf "jump %s" (X86.string_of_lbl a)
  | If (a0,op,a2,l1,l2) -> sprintf "if (%s %s %s) then %s else %s"
      (string_of_operand a0) (string_of_compop op) (string_of_operand a2)
      (X86.string_of_lbl l1) (X86.string_of_lbl l2)

let string_of_bb b =
  sprintf "  block %s {\n    %s\n    %s\n  }"
      (X86.string_of_lbl b.bb_lbl)
      (String.concat "\n    " (List.map string_of_insn  b.bb_body))
      (string_of_cfinsn b.bb_link)

let string_of_fdecl {il_name=fid; il_tmps=tmps; il_entry=l; il_cfg=blocks} =
  sprintf "fdecl %s [%s] @%s {\n%s\n}"
    fid (String.concat ", " (List.map string_of_uid tmps))
    (X86.string_of_lbl l) (String.concat "\n\n" (List.map string_of_bb blocks))

let string_of_prog {il_globals=globals; il_fdecls=fdecls; il_start=main} =
  sprintf "prog@%s\n%s\n%s" main
    (List.fold_left (fun s -> fun g -> s ^ Cunit.string_of_global_data g) "" globals)
    (List.fold_left (fun s -> fun f -> s ^ string_of_fdecl f) "" fdecls) 

(* il2c *)
let c_string_of_binop = function 
  | Plus -> "+" | Times -> "*" | Minus -> "-"
  | And -> "&" | Or -> "|" | Xor -> "^"
  | Compare o -> string_of_compop o 
  | _ -> failwith "C does not support them."
      
let c_string_of_insn = function
  | BinArith (a0,op,a1) ->  
      (string_of_operand a0) ^ " = " ^
      begin match op with
        | Shl -> sprintf " shl(%s, %s);" 
          (string_of_operand a0) (string_of_operand a1)
        | Shr -> sprintf " shr(%s, %s);" 
          (string_of_operand a0) (string_of_operand a1)
        | Sar -> sprintf " sar(%s, %s);"
          (string_of_operand a0) (string_of_operand a1)
        | Move -> sprintf "%s;"
          (string_of_operand a1)
	| _ -> sprintf " %s %s %s;" 
          (string_of_operand a0) (c_string_of_binop op) (string_of_operand a1)
      end
  | UnArith (op,a0) -> sprintf "%s = %s%s;" (string_of_operand a0)
      (string_of_unop op) (string_of_operand a0)
  | Alloc (a0, a1) -> sprintf "%s = (int)malloc (%s);" (string_of_operand a0)
      (string_of_operand a1)                   
  | AddrOf (a0, a1, a2) -> sprintf "%s = %s + %s;" (string_of_operand a0)
      ((match a1 with
         | Imm _ -> failwith "Cannot get address of Imm"
         | _ -> "(int)&") ^ (string_of_operand a1)) 
      (string_of_operand a2)  
  | Load (a0, a1) -> sprintf "%s = *((int*)%s);" (string_of_operand a0)
      (string_of_operand a1)                                      
  | Store (a0, a1) -> sprintf "*((int*)%s) = %s;" (string_of_operand a0)
      (string_of_operand a1)                                      
  | Call (opa0, fid, as1) ->
      match opa0 with
        | None -> sprintf "%s (%s);" 
           fid (String.concat ", " (List.map string_of_operand as1))
        | Some a0 -> sprintf "%s = %s (%s);" (string_of_operand a0) 
           fid (String.concat ", " (List.map string_of_operand as1))

let c_string_of_cfinsn = function
  | Ret (Some o) -> sprintf "return %s;" (string_of_operand o)
  | Ret None -> sprintf "return;"
  | Jump a -> sprintf "goto %s;" (X86.string_of_lbl a)
  | If (a0,op,a2,l1,l2) -> sprintf "if (%s %s %s) goto %s; else goto %s;"
      (string_of_operand a0) (string_of_compop op) (string_of_operand a2)
      (X86.string_of_lbl l1) (X86.string_of_lbl l2)

let c_string_of_bb b =
  sprintf "  %s: {\n    %s\n    %s\n  }"
      (X86.string_of_lbl b.bb_lbl)
      (String.concat "\n    " (List.map c_string_of_insn  b.bb_body))
      (c_string_of_cfinsn b.bb_link)

let rec c_string_of_args na =
  if (na <= 0) then ""
  else if (na = 1) then "int arg0"
  else c_string_of_args (na-1) ^ ",int  arg" ^ string_of_int (na-1)

let c_string_of_fdecl 
  {il_name=fid; il_num_of_args=na; il_tmps=tmps; il_cfg=blocks} =
  sprintf "int %s (%s) {\n%s \n %s\n}"
    fid (c_string_of_args na)
    (String.concat "\n" 
      (List.map (fun u -> "int " ^ string_of_uid u ^ " =0;" ) tmps))
    (String.concat "\n\n" (List.map c_string_of_bb blocks))

let c_string_of_fdecl_header 
  {il_name=fid; il_num_of_args=na; il_tmps=tmps; il_cfg=blocks} =
  sprintf "int %s (%s);" fid (c_string_of_args na)

let quote_asm_string s =
  let outbuf = Buffer.create (String.length s) in
  Buffer.add_char outbuf '\"';
  String.iter (function
    | '\n' -> Buffer.add_string outbuf "\\n"
    | '\"' -> Buffer.add_string outbuf "\\\""
    | '\\' -> Buffer.add_string outbuf "\\\\"
    | '\t' -> Buffer.add_string outbuf "\\t"
    | c -> Buffer.add_char outbuf c
    ) s;
  Buffer.add_string outbuf "\\0\"";
  Buffer.contents outbuf

let c_string_of_global_data {Cunit.value=v;Cunit.label=l} =
  let chars_of_string str : string =
    let rec _char_list_of_string index chars str : string list =
      if index < 0 then chars
      else _char_list_of_string (index-1) 
        (("'"^(Char.escaped str.[index])^"'")::chars) str
    in
    "{" ^
    (String.concat "," (_char_list_of_string (String.length str-1) [] str)) ^ 
    "}"
  in
  match v with
  | Cunit.GSafeStringz s -> sprintf "int %s = %d;\nchar %sdata[%d] = %s;\n" 
      (X86.string_of_lbl l) (String.length s) 
      (X86.string_of_lbl l) (String.length s) 
      (chars_of_string s)
  | Cunit.GStringz s -> sprintf "char %s[%d] = %s;\n" 
      (X86.string_of_lbl l) (String.length s) 
      (chars_of_string s)
  | Cunit.GInt32 v -> sprintf "int %s = %s;\n" (X86.string_of_lbl l)
      (Int32.to_string v)
  | Cunit.GZero z -> sprintf "char %s[%d] = {};\n" (X86.string_of_lbl l) z
  | Cunit.GExtern -> ""

let _c_string_of_prog {il_globals=globals; il_fdecls=fdecls} =
  sprintf "%s\n%s\n%s"
    (List.fold_left (fun s -> fun f -> s ^ c_string_of_fdecl_header f) "" fdecls) 
    (List.fold_left 
       (fun s -> fun g -> s ^ c_string_of_global_data g) "" (List.rev globals))
    (List.fold_left (fun s -> fun f -> s ^ c_string_of_fdecl f) "" fdecls) 

let c_string_of_prog (p:prog) =
  sprintf 
"#include <stdio.h>\n
#include <stdlib.h>\n
int32_t shr(int32_t e, int32_t amt) {
  unsigned r = (*(unsigned*)&e) >> (amt%%32);
  return (*(int32_t*)&r);
}
int32_t shl(int32_t e, int32_t amt) {
  return e<<(amt%%32);
}
int32_t sar(int32_t e, int32_t amt) {
  return e>>(amt%%32);
}
%s\n"
  (_c_string_of_prog p)
 
let c_run_prog (p:prog) (args:string) : string = 
  let ccode = c_string_of_prog p in
  let _ = if (!Platform.verbose_on) then
    Printf.printf "compiling:\n%s\n" ccode
    else (print_char '.'; flush stdout) in
  let tmp_dot_c = Platform.gen_name (!Platform.obj_path) "tmp" ".c" in
  let tmp_exe   = 
    Platform.gen_name (!Platform.bin_path) "tmp" (Platform.executable_exn) in
  let tmp_out   = tmp_exe ^ ".out" in
  let _ = 
    if (!Platform.verbose_on) then
      Printf.printf "* TMP FILES:\n*  %s\n*  %s\n" tmp_dot_c tmp_exe 
      else () in
  let fout = open_out tmp_dot_c in
    try
      output_string fout ccode;
      close_out fout;
      Platform.link [tmp_dot_c] tmp_exe;
      Platform.run_program args tmp_exe tmp_out
    with
     | Platform.AsmLinkError(s1, s2) -> failwith (Printf.sprintf "%s\n%s" s1 s2) 
