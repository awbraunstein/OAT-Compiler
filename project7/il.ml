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
  | Call of operand option * string * operand list  (* a0 := call f1 a2 *)
  | Method of operand option * operand * int * operand list  
     (* a0 := call a1.i2 (a1::a3) 
      * a0 is return
      * a1 is the pointer to the class value
      * i2 is the offset of the method
      * a3 is the original arguments of the method, Phase2 should
      *   take (a1::a3) as the arguments for X86 code.
     *)

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

type efdecl = {
  efd_name: string;      (* The name of this function. *)
  efd_num_of_args: int;  (* For debugging: the number of arguments. *)
}  

type fdecl = {
  fd_name: string;      (* The name of this function. *)
  fd_num_of_args: int;  (* For debugging: the number of arguments. *)
  fd_tmps:  uid list;   (* List of slot identifiers used in this function. *)
  fd_entry:  lbl;       (* The entry label of this function. *)
  fd_cfg:   bb list;    
}  
  
type cdecl = {
  ct_entry: lbl;             (* The entry label of this class. *)
  ct_super: lbl option;      (* The entry label of super class if there is. *)
  ct_dispatch_lbl: lbl;      (* The label of ct_dispatch_tbl *)
  ct_dispatch_tbl: lbl list; (* Dispatch table *)
}
  
type prog = {
  il_globals: Cunit.global_data list;
  il_efdecls: efdecl list;
  il_fdecls: fdecl list;
  il_cdecls: cdecl list;
  il_start: string 
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
      (match opa0 with
        | None -> sprintf "call %s (%s)" 
           fid (String.concat ", " (List.map string_of_operand as1))
        | Some a0 -> sprintf "%s := call %s (%s)" (string_of_operand a0) 
           fid (String.concat ", " (List.map string_of_operand as1)))
  | Method (opa0, a1, i2, as3) ->
      (match opa0 with
        | None -> sprintf "call %s.%d (%s);" 
           (string_of_operand a1) i2
           (String.concat ", " (List.map string_of_operand as3))
        | Some a0 -> sprintf "%s = call %s.%d (%s);" 
           (string_of_operand a0) (string_of_operand a1) i2
           (String.concat ", " (List.map string_of_operand as3)))

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

let string_of_efdecl {efd_name=fid} =
  sprintf "efdecl %s\n" fid

let string_of_fdecl {fd_name=fid; fd_tmps=tmps; fd_entry=l; fd_cfg=blocks} =
  sprintf "fdecl %s [%s] @%s {\n%s\n}\n"
    fid (String.concat ", " (List.map string_of_uid tmps))
    (X86.string_of_lbl l) (String.concat "\n\n" (List.map string_of_bb blocks))

let string_of_cdecl 
  {ct_entry=this; ct_super=super_opt; 
   ct_dispatch_lbl=tbl_lbl; ct_dispatch_tbl=tbl} =
  sprintf "cdecl:%s\n  %s\n%s:\n  [%s]\n"
    (X86.string_of_lbl this)
    (match super_opt with
       | None -> "none" 
       | Some super -> X86.string_of_lbl super)
    (X86.string_of_lbl tbl_lbl)
    (String.concat ", " (List.map X86.string_of_lbl tbl))

let string_of_prog 
  {il_globals=globals; il_efdecls=efdecls; il_fdecls=fdecls; il_cdecls=cdecls; 
   il_start=main} =
  sprintf "prog@%s\n%s\n%s\n%s\n%s" main
    (List.fold_left (fun s -> fun g -> s ^ Cunit.string_of_global_data g) "" 
       globals)
    (List.fold_left (fun s -> fun c -> s ^ string_of_cdecl c) "" 
       (List.rev cdecls)) 
    (List.fold_left (fun s -> fun f -> s ^ string_of_efdecl f) "" efdecls) 
    (List.fold_left (fun s -> fun f -> s ^ string_of_fdecl f) "" fdecls) 

(* il2c *)
let c_string_of_binop = function 
  | Plus -> "+" | Times -> "*" | Minus -> "-"
  | And -> "&" | Or -> "|" | Xor -> "^"
  | Compare o -> string_of_compop o 
  | _ -> failwith "C does not support them."

let c_string_of_castin fid = 
  if (fid = "length_of_array" ||
      fid = "string_of_array" ||
      fid = "array_of_string" ||
      fid = "print_string" ||
      fid = "string_cat" ||
      fid = "int_of_string" ||
      fid = "bool_of_string" ||
      fid = "length_of_string" ||
      fid = "write_file" ||
      fid = "read_file")
  then "(int*)"
  else ""

let c_string_of_castout fid = 
  if (fid = "string_of_array" ||
      fid = "array_of_string" ||
      fid = "alloc_array" ||
      fid = "string_cat" ||
      fid = "string_of_int" ||
      fid = "string_of_bool" ||
      fid = "string_alloc" ||
      fid = "read_file")
  then "(int)"^fid
  else fid
      
let c_string_of_call fid as1 =
  sprintf "%s (%s)" (c_string_of_castout fid) 
    (begin match fid with
      | "string_at" ->
        (match as1 with
          | a1::a2::[] -> 
              "(int*)" ^
              string_of_operand a1 ^
              ", " ^
              string_of_operand a2
          | _ -> failwith "Internal error: string_at has wrong numbers of args.")
      | "string_set" ->
        (match as1 with
          | a1::a2::a3::[] -> 
              "(int*)" ^
              string_of_operand a1 ^
              ", " ^
              string_of_operand a2 ^
              ", " ^
              string_of_operand a3
          | _ -> failwith "Internal error: string_set has wrong numbers of args.")
      | _ ->
        (String.concat ", " (List.map 
          (fun a -> c_string_of_castin fid ^ string_of_operand a) as1))
    end)
  
(* Generate a fresh name *)
let mk_ivar : unit -> string =
  let ctr = ref 0 in
    fun () -> let c = !ctr in ctr := !ctr + 1; "_itmp" ^ (string_of_int c)

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
      (match opa0 with
        | None -> sprintf "%s;" (c_string_of_call fid as1) 
        | Some a0 -> sprintf "%s = %s;" (string_of_operand a0) 
            (c_string_of_call fid as1)) 
  | Method (opa0, a1, i2, as3) ->
      let pars = String.concat ", " (List.map (fun _ ->"int") ((Arg 0)::as3)) in
      let sep = (if List.length as3 = 0 then "" else ", ") in
      let args = String.concat ", " (List.map string_of_operand as3) in 
      (match opa0 with
        | None -> ""
        | Some a0 -> sprintf "%s = " (string_of_operand a0)) ^
      Printf.sprintf
        "((int(*)(%s))(((int*)(*((int*)(%s-4))))[%i])) (%s%s%s);"
        pars (string_of_operand a1) i2 (string_of_operand a1) sep args

let c_string_of_cfinsn = function
  | Ret (Some o) -> sprintf "return %s;" (string_of_operand o)
  | Ret None -> sprintf "return 0;"
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

let c_string_of_efdecl {efd_name=fid; efd_num_of_args=na} =
  sprintf "extern int %s (%s);\n" fid (c_string_of_args na)

let c_string_of_fdecl 
  {fd_name=fid; fd_num_of_args=na; fd_tmps=tmps; fd_cfg=blocks} =
  sprintf "int %s (%s) {\n%s \n %s\n}\n"
    fid (c_string_of_args na)
    (String.concat "\n" 
      (List.map (fun u -> "  int " ^ string_of_uid u ^ " =0;" ) tmps))
    (String.concat "\n\n" (List.map c_string_of_bb blocks))

let c_string_of_fdecl_header 
  {fd_name=fid; fd_num_of_args=na; fd_tmps=tmps; fd_cfg=blocks} =
  sprintf "int %s (%s);\n" fid (c_string_of_args na)

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
  | Cunit.GStringz s -> sprintf "char %s[%d] = %s;\n" (X86.string_of_lbl l) 
      (String.length s) (chars_of_string s)
  | Cunit.GLabelOffset (l1, i2) -> sprintf "int %s = &%s+%s;\n" 
      (X86.string_of_lbl l) (X86.string_of_lbl l1) (Int32.to_string i2)
  | Cunit.GLabels ls -> sprintf "int* %s[%d] = {%s};\n" (X86.string_of_lbl l)
      (List.length ls) (String.concat ", " 
        (List.map (fun l -> "(int*)&" ^ (X86.string_of_lbl l)) ls))
  | Cunit.GInt32 v -> sprintf "int %s = %s;\n" (X86.string_of_lbl l)
      (Int32.to_string v)
  | Cunit.GZero z -> sprintf "char %s[%d] = {};\n" (X86.string_of_lbl l) z
  | Cunit.GExtern -> ""

let c_string_of_cdecl 
  {ct_entry=this; ct_super=super_opt; ct_dispatch_tbl=tbl} =
  sprintf "int %s[%d] = {%s%s %s};\n"
    (X86.string_of_lbl this) (1+List.length tbl) 
    (match super_opt with
       | None -> "0" 
       | Some super -> "(int)&("^X86.string_of_lbl super^"[1])")
    (if List.length tbl = 0 then "" else ", (int)&")
    (String.concat ", (int)&" (List.map X86.string_of_lbl tbl))

let _c_string_of_prog {il_globals=globals; il_efdecls=efdecls; 
  il_fdecls=fdecls; il_cdecls=cdecls} =
  sprintf "%s\n%s\n%s\n%s\n%s"
    (List.fold_left (fun s -> fun f -> s ^ c_string_of_fdecl_header f) "" fdecls) 
    (List.fold_left (fun s -> fun c -> s ^ c_string_of_cdecl c) "" 
       (List.rev cdecls)) 
    (List.fold_left 
       (fun s -> fun g -> s ^ c_string_of_global_data g) "" (List.rev globals))
    (List.fold_left (fun s -> fun f -> s ^ c_string_of_efdecl f) "" efdecls) 
    (List.fold_left (fun s -> fun f -> s ^ c_string_of_fdecl f) "" fdecls) 

let c_string_of_prog (p:prog) =
  sprintf 
"#include <stdio.h>\n
#include <stdlib.h>\n

extern int* oat_malloc(int size);
extern void oat_abort(int errno);
extern int length_of_array (int *arr);
extern int* array_of_string (int *str);
extern int* string_of_array (int *arr);
extern void print_string (int* str);
extern void print_int (int i);
extern void print_bool (int i);
extern int* alloc_array (int size);
extern int* string_alloc (int size);
extern int length_of_string (int* str);
extern int* string_of_int (int i);
extern int int_of_string (int* str);
extern int* string_of_bool (int b);
extern int bool_of_string (int* str);
extern int int_of_bool (int b);
extern int bool_of_int (int i);
extern int string_at (int* s, int i);
extern void string_set (int* s, int i, int c);
extern int* string_cat (int* l, int* r);

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
