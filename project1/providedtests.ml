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
      Call (Lbl (mk_lbl_named "j"));
      Add (eax, Imm 4l);
      Ret
    ]);]
   );
  ("test-j", run_test 7l
    [(mk_insn_block (mk_lbl_named "j") [
      Sub (eax, Imm 1l);
      Ret
    ]); 
     (mk_insn_block (mk_lbl_named "main") [
      Add (eax, Imm 4l);
      Call (Lbl (mk_lbl_named "j"));
      Add (eax, Imm 4l);
      Ret
    ]);]
   )
  ])
]
