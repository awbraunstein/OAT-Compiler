open Assert
open Interpreter
open X86


(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)
let run_test (ans:int32) (code:X86.insn_block list) () =
  let res = Interpreter.run code in
    if res = ans then () else failwith 
    (Printf.sprintf("Expected %lx got %lx") ans res)
(*Computes the nth fib number*)
let provided_tests : suite = [
  Test("Student-Provided Big Test for Part II", 
   [
   ("fib 6", run_test 5l
    [(mk_insn_block (mk_lbl_named "fib") [
      Add (ebx, eax);
      Neg (ebx);
      Add (eax, ebx);
      Sub (ecx, Imm 1l);
      Cmp (ecx, Imm 0l);
      J (Sgt, mk_lbl_named "fib");
      Ret
]);
     (mk_insn_block (mk_lbl_named "main") [
      Push ebp;
      Mov (ebp, esp);
      Push ebx; 
      Push ecx;
      Mov (ecx, Imm 6l);
      Mov (ebx, Imm 0l); 
      Mov (eax, Imm 1l); 
      Jmp (Lbl (mk_lbl_named "fib"));
      Ret
    ]);
    ]);
   ])
  ]