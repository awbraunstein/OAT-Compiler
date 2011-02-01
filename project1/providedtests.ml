open Assert
open Interpreter
open X86
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite = [
  Test("Control-Flow", 
   [("test-call", run_test 7l
    [(mk_insn_block (mk_lbl_named "call") [
      Sub (eax, Imm 1l);
      Ret
    ]); 
     (mk_insn_block (mk_lbl_named "main") [
      Add (eax, Imm 4l);
      Call (Lbl (mk_lbl_named "call"));
      Add (eax, Imm 4l);
      Ret
    ]);]
   );
  ("test-j", run_test 0l
    [(mk_insn_block (mk_lbl_named "j") [
      Sub (eax, Imm 1l);
      J (Sgt, (mk_lbl_named "j"));
      Ret
    ]); 
     (mk_insn_block (mk_lbl_named "main") [
      Add (eax, Imm 4l);
      J (Sgt, (mk_lbl_named "j"));
      Add (eax, Imm 4l);
      Ret
    ]);]
   );
  
   ("fib 7", run_test 8l
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
    
    
    ("test-push/pop", run_test 1l
    [(mk_insn_block (mk_lbl_named "main") [
      Add (eax, Imm 4l);
      Push(eax);
      Push(Imm 1l);
      Pop(eax);
      Ret
    ]);]
   );
   ("test-Imul", run_test 12l
    [(mk_insn_block (mk_lbl_named "main") [
      Add (eax, Imm 3l);
      Add (ebx, Imm 4l);
      Imul (Eax, ebx);
      Ret
    ]);]
   );
  ("fact 4", run_test 24l
    [(mk_insn_block (mk_lbl_named "fact") [
      Push (ebp);
      Mov (ebp, esp);
      Mov (eax, (stack_offset 8l));
      Cmp (eax, Imm 0l);
      J (Sgt, (mk_lbl_named "fact_recurse"));
      Mov (eax, Imm 1l);
      Pop (ebp);
      Ret
    ]); 
     (mk_insn_block (mk_lbl_named "fact_recurse") [
      Sub (eax, Imm 1l);
      Push (eax);
      Call (Lbl (mk_lbl_named "fact"));
      Add (esp, (Imm 4l));
      Mov (ebx, (stack_offset 8l));
      Imul (Eax, ebx);
      Pop (ebp);
      Ret
    ]); 
     (mk_insn_block (mk_lbl_named "main") [
      Push (Imm 4l);
      Call (Lbl (mk_lbl_named "fact"));
      Add (esp, (Imm 4l));
      Ret
    ]);]
    );
    ("test-cmp", run_test 4l
    [(mk_insn_block (mk_lbl_named "cmp") [
      Add (eax, Imm 4l);
      Ret
    ]); 
     (mk_insn_block (mk_lbl_named "main") [
      Cmp (Imm 1l, Imm 0l);
      J (Sgt, (mk_lbl_named "cmp"));
      Add (eax, Imm 3l);
      Ret
    ]);]
   )
  ])
]
