(* CIS341: Project 1 Interpreter *)
(* Author: Steve Zdancewic *)
open X86

(* Int32 / Int64 abbreviations and infix arithmetic *)
let (+@) = Int32.add
let (-@) = Int32.sub
let (/@) = Int32.div
let ( *@ ) = Int32.mul
let (<@) a b = (Int32.compare a b) < 0
let (<=@) a b = (Int32.compare a b) <= 0
let (>@) a b = (Int32.compare a b) > 0
let (>=@) a b = (Int32.compare a b) >= 0
let (<@@) a b = (Int64.compare a b) < 0

exception X86_segmentation_fault of string

(* Interpret the registers as indices into the register file array *)
let eaxi = 0
let ebxi = 1
let ecxi = 2
let edxi = 3
let esii = 4
let edii = 5
let ebpi = 6
let espi = 7 

let get_register_id = function
  | Eax -> eaxi 
  | Ebx -> ebxi 
  | Ecx -> ecxi 
  | Edx -> edxi 
  | Esi -> esii 
  | Edi -> edii 
  | Ebp -> ebpi
  | Esp -> espi


let mem_size = 1024                 (* Size of memory in words *)
let mem_top : int32 = 0xfffffffcl   (* Last addressable memory location *)
let mem_bot : int32 =               (* First addressable memory location *)
	  (Int32.of_int (mem_size * 4)) *@ (-1l)

(* 
   Maps virtual addresses (int32 addresses) to physical addresses (int indices). 
   Raises an X86_segmentation_fault exception if the provided virtual address 
   does not map or if the address is unaligned. 
*)
let map_addr (addr:int32) : int =
  if (addr >=@ mem_bot) && (addr <=@ mem_top) &&
      (Int32.logand addr 3l = 0l)
      then ( Int32.to_int ((addr -@ mem_bot) /@ 4l))
  else ( raise (X86_segmentation_fault "memory not mapped or aligned"))

type x86_state = {
    s_mem : int32 array;    (* 1024 32-bit words -- the heap *)
    s_reg : int32 array;    (* 8 32-bit words -- the register file *)
    mutable s_OF : bool;    (* overflow flag *)
    mutable s_SF : bool;    (* sign bit flag *)
    mutable s_ZF : bool;    (* zero flag *)
}

let mk_init_state () : x86_state = 
  let xs = {
  s_mem = Array.make mem_size 0l;
  s_reg = Array.make 8 0l;
  s_OF  = false;
  s_SF  = false;
  s_ZF  = false;
  } in
  xs.s_reg.(espi) <- mem_top; xs 

let print_state (xs:x86_state) : unit =
  (Array.iter (fun e -> Printf.printf "%lx " e) xs.s_mem);
  (Printf.printf "\neax: %lx ebx: %lx ecx: %lx edx: %lx" xs.s_reg.(eaxi)
      xs.s_reg.(ebxi) xs.s_reg.(ecxi) xs.s_reg.(edxi));
  (Printf.printf "\nesi: %lx edi: %lx ebp: %lx esp: %lx" xs.s_reg.(esii)
      xs.s_reg.(edii) xs.s_reg.(ebpi) xs.s_reg.(espi));
  (Printf.printf "\nOF: %b SF: %b ZF: %b\n" xs.s_OF xs.s_SF xs.s_ZF)
  
  
let condition_matches (xs:x86_state) (c:X86.cnd) : bool =
  begin match c with
    | Slt -> (xs.s_SF != xs.s_OF)
    | Sge -> (xs.s_SF = xs.s_OF)
    | Sle -> ((xs.s_SF != xs.s_OF) || xs.s_ZF)
    | Sgt -> (not ((xs.s_SF != xs.s_OF) || xs.s_ZF))
    | Eq -> xs.s_ZF
    | NotEq -> (not xs.s_ZF)
    | Zero -> xs.s_ZF
    | NotZero -> (not xs.s_ZF)
  end

(* Returns the bit at a given index in a 32-bit word as a boolean *)
let get_bit bitidx n =
  let shb = Int32.shift_left 1l bitidx in
  Int32.logand shb n = shb  

let rec get_block(code:insn_block list)(l:lbl): insn_block = 
  begin match code with
    | [] -> raise (X86_segmentation_fault "FAIL!")
    | h::tl -> if h.label = l then h else get_block tl l
  end
   
  
let do_command(i:insn) (xs:x86_state) : unit =
  begin match i with
    | Add (d,s) -> 
      begin match (d,s) with
        | (Reg x, Reg y) -> xs.s_reg.(get_register_id x) <- 
          xs.s_reg.(get_register_id x) +@ xs.s_reg.(get_register_id y);
        | (Reg x, Imm y) -> xs.s_reg.(get_register_id x) <- 
          xs.s_reg.(get_register_id x) +@ y;
        | (Imm x, _) -> raise (X86_segmentation_fault "FAIL!")
        | (Lbl x, _) -> raise (X86_segmentation_fault "FAIL!")
        | (Ind x, Imm y) -> ()
        | (Ind x, Reg y) -> ()
        | (Ind x, Ind y) -> ()
        | (Ind x, Lbl y) -> ()
        | (Reg x, Ind y) -> raise (X86_segmentation_fault "FAIL!")
        | (Reg x, Lbl y) -> raise (X86_segmentation_fault "FAIL!")
      end
    | Neg o ->
      begin match o with
        | Reg x -> xs.s_reg.(get_register_id x) <-  Int32.neg(xs.s_reg.(get_register_id x))
        | Imm x -> raise (X86_segmentation_fault "FAIL!")
        | Lbl x -> raise (X86_segmentation_fault "FAIL!")
        | Ind x -> raise (X86_segmentation_fault "FAIL!")
      end
    | Sub (d,s) -> raise (X86_segmentation_fault "unimplemented")
    | Lea (d,s) -> raise (X86_segmentation_fault "unimplemented")
    | Mov (d,s) ->
      begin match (d,s) with
        | (Reg x, Reg y) -> xs.s_reg.(get_register_id x) <- 
          xs.s_reg.(get_register_id x) +@ xs.s_reg.(get_register_id y);
        | (Reg x, Imm y) -> raise (X86_segmentation_fault "FAIL!")
        | (Imm x, _) -> raise (X86_segmentation_fault "FAIL!")
        | (Lbl x, _) -> raise (X86_segmentation_fault "FAIL!")
        | (Ind x, _) -> raise (X86_segmentation_fault "FAIL!")
        | (Reg x, Ind y) -> raise (X86_segmentation_fault "FAIL!")
        | (Reg x, Lbl y) -> raise (X86_segmentation_fault "FAIL!")
      end
    | Shl (d,s) -> ()
    | Sar (d,s) -> ()
    | Shr (d,s) -> ()
    | Not o     -> ()
    | And (d,s) -> ()
    | Or (d,s)  -> ()
    | Xor (d,s) -> ()
    | Push o    -> ()
    | Pop o     -> ()
    | Cmp (c1,c2)    -> ()
    | Setb (dest,cc) -> ()
    | Jmp o     -> ()
    | Call o    -> ()
    | Ret       -> ()
    | J (cond,lbl) -> ()
    | Imul (d,s) -> ()
  end

let rec get_insns(i:insn list)(xs:x86_state):unit =
  begin match i with
    | [] -> ()
    | h::tl -> do_command h xs; get_insns tl xs
  end
  
let interpret (code:insn_block list) (xs:x86_state) (l:lbl) : unit =
 let block = get_block code l in
     get_insns block.insns xs

let run (code:insn_block list) : int32 =
  let main = X86.mk_lbl_named "main" in
  let xs = mk_init_state () in
  let _ = interpret code xs main in
    xs.s_reg.(eaxi)
      
