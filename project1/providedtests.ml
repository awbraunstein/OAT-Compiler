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
  
   ("fib 7", run_test 5l
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
    ("lea", st_test "mem(eax) = 1021" [(mk_block "main"  [
           Push (Imm 6l);
           Push (Imm 12l);
           Push (Imm 1l);
     Lea (Eax, { i_base = Some Esp; i_iscl = None; i_disp = Some (DImm 4l)});
     Ret
   ])] 
    (fun state -> map_addr (state.s_reg.(0)) = 1021));
       
   ("setb", st_test "eax = 1; ebx = 12; ecx = -6" [(mk_block "main"  [
           Push (Imm 6l);
           Mov (eax, stack_offset 0l);
           Push (Imm 12l);
           Mov (ebx, stack_offset 0l);
           Mov (ecx, eax);
           Sub (ecx, ebx);
           Setb (eax, Slt);
     Ret
   ])] 
    (fun state -> state.s_reg.(0) = 1l && 
                   state.s_reg.(1) = 12l && 
                   state.s_reg.(2) = 0xfffffffal));
   
   ("sar", st_test "eax = 1, ebx = 9, ecx = 0" [(mk_block "main"  [
           Push (Imm 6l);
           Mov (eax, stack_offset 0l);
           Sar (eax, Imm 2l);
           Push (Imm 77l);
           Mov (ebx, stack_offset 0l);
           Sar (ebx, Imm 3l);
           Push (Imm (-1l));
           Mov (ecx, stack_offset 0l);
           Sub (ecx, Imm 0xffffffffl);
     Ret
   ])] 
    (fun state -> state.s_reg.(0) = 1l && 
                   state.s_reg.(1) = 9l && 
                   state.s_reg.(2) = 0l));
       
       ("shr", st_test "eax = 1, ebx = 9, ecx = 1" [(mk_block "main"  [
           Push (Imm 6l);
           Mov (eax, stack_offset 0l);
           Shr (eax, Imm 2l);
           Push (Imm 77l);
           Mov (ebx, stack_offset 0l);
           Shr (ebx, Imm 3l);
           Push (Imm (-1l));
           Mov (ecx, stack_offset 0l);
           Shr (ecx, Imm 31l);
     Ret
   ])] 
    (fun state -> state.s_reg.(0) = 1l && 
                   state.s_reg.(1) = 9l && 
                   state.s_reg.(2) = 1l));
                   
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