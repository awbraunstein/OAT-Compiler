open Assert
open X86
open Ast
open Range

(* Do NOT modify this file -- we will overwrite it with our *)
(* own version when we test your project.                   *)

(* These tests will be used to grade your assignment *)

let test_path = ref "tests/"

let assert_bool (s:string) (b:bool) : unit =
  if b then () else failwith s

let ast_test (s:string) (a:Range.t Ast.prog) () : unit =
  let ast = Phase1.parse "ast_test" (Lexing.from_string s) in
    if ast = a then () else failwith (Printf.sprintf "bad parse of \"%s\"" s)

let parse_error_test (s:string) (expected:exn) () : unit =
  try 
    let _ = Phase1.parse "stdin" (Lexing.from_string s) in
      failwith (Printf.sprintf "String \"%s\" should not parse." s)
  with
    | e -> if e = expected then () else
	failwith (Printf.sprintf "Lexing/Parsing \"%s\" raised the wrong exception." s)

let comp_test ?ast2c:(ast2c=true) ?il2c:(il2c=true)
  (prog:Range.t Ast.prog) (args:string) (ans:string) () : unit =
  let _ = if (!Platform.verbose_on) then
    Printf.printf "compiling:\n%s\n" (Astlib.string_of_prog prog)
  else (print_char '.'; flush stdout) in
  let tmp_dot_s = Platform.gen_name (!Platform.obj_path) "tmp" ".s" in
  let tmp_dot_o = Platform.gen_name (!Platform.obj_path) "tmp" ".o" in
  let tmp_exe   = Platform.gen_name (!Platform.bin_path) "tmp" (Platform.executable_exn) in
  let tmp_out   = tmp_exe ^ ".out" in
  let _ = if (!Platform.verbose_on) then
    Printf.printf "* TMP FILES:\n*  %s\n*  %s\n*  %s\n" tmp_dot_s tmp_dot_o tmp_exe 
  else () in
  let _ = Tc.typecheck_prog prog in
  let _ = if ast2c then
    try begin
      let result = Astlib.c_run_prog prog args in 
      if result = ans then ()
      else failwith (Printf.sprintf "AST2C output %s expected %s" result ans)
    end with
      | Platform.AsmLinkError(s1, s2) -> failwith (Printf.sprintf "%s\n%s" s1 s2)
  in
  let prog_il = Phase1.compile_prog prog in
  let _ = if il2c then
    try begin
      let result = Il.c_run_prog prog_il args in 
      if result = ans then ()
      else failwith (Printf.sprintf "IL2C output %s expected %s" result ans)
    end with
      | Platform.AsmLinkError(s1, s2) -> failwith (Printf.sprintf "%s\n%s" s1 s2)
  in
  let cu = Phase2.compile_prog prog_il in
  let fout = open_out tmp_dot_s in
    begin
      Cunit.output_cunit cu fout;
      close_out fout;
      Platform.assemble tmp_dot_s tmp_dot_o;
      Platform.link [tmp_dot_o] tmp_exe;
      try
        let result = Platform.run_program args tmp_exe tmp_out in
        if result = ans then ()
        else failwith (Printf.sprintf "Program output %s expected %s" result ans)
      with | Platform.AsmLinkError(s1, s2) -> 
        failwith (Printf.sprintf "%s\n%s" s1 s2)
    end

let file_test ?ast2c:(ast2c=true) ?il2c:(il2c=true) 
  (fn:string) (args:string) (ans:string) () : unit =
  let path = !test_path ^ fn in
  let buffer = open_in path in
  let prog = Phase1.parse fn (Lexing.from_channel buffer) in
  let _ = close_in buffer in
    comp_test ~ast2c:ast2c ~il2c:il2c prog args ans ()

let file_error_test ?ast2c:(ast2c=true) ?il2c:(il2c=true) 
  (fn:string) (args:string) () : unit =
  let path = !test_path ^ fn in
  let buffer = open_in path in
  let prog = Phase1.parse fn (Lexing.from_channel buffer) in
  let _ = close_in buffer in
  try
    (comp_test ~ast2c:ast2c ~il2c:il2c prog args "" ());
    failwith 
      (Printf.sprintf "File \"%s\" should have typecheck/runtime errors." fn)
  with
    | _ -> ()

let file_parse_test (fn:string) (ans:Range.t Ast.prog) () : unit =
  let path = !test_path ^ fn in
  let buffer = open_in path in
  let prog = Phase1.parse fn (Lexing.from_channel buffer) in
  let _ = close_in buffer in
    if Astlib.eq_prog prog ans then () 
    else failwith (Printf.sprintf "bad of \"%s\"" fn)

let file_parse_error_test (fn:string) (expected:exn) () : unit =
  let passed = 
    try 
      let path = !test_path ^ fn in
      let buffer = open_in path in
      let _ = Phase1.parse fn (Lexing.from_channel buffer) in
      true
    with
      | _ -> false 
  in 
    if passed then
      failwith (Printf.sprintf "File \"%s\" should not parse." fn)
    else ()

let file_tc_error_test (fn:string) (expected:exn) () : unit =
  let passed = 
    try 
      let path = !test_path ^ fn in
      let buffer = open_in path in
      let prog = Phase1.parse fn (Lexing.from_channel buffer) in
      let _ = Tc.typecheck_prog prog in
      true
    with
      | _ -> false 
  in 
    if passed then
      failwith (Printf.sprintf "File \"%s\" should not parse." fn)
    else ()



let typechecking_tests : suite = [
  GradedTest("Typechecking error tests", 20, [   
    ("error1_err", (file_tc_error_test "error1.oat" (Failure "error1.oat:[2.29-2.30]: j is not declared.")));
    ("error2_err", (file_tc_error_test "error2.oat" (Failure "redeclaraion of an argument")));
    ("error3_err", (file_tc_error_test "error3.oat" (Failure "redeclaraion of a global variable")));
    ("error4_err", (file_tc_error_test "error4.oat" (Failure "redeclaraion of a function")));
    ("error5_err", (file_tc_error_test "error5.oat" (Failure "redeclaraion of a local variable")));
    ("error7_err", (file_tc_error_test "error7.oat" (Failure "a function and a global have the same name")));
    ("error8_err", (file_tc_error_test "error8.oat" (Failure "a function and an argument have the same name")));
    ("error9_err", (file_tc_error_test "error9.oat" (Failure "a function and a local have the same name")));
    ("error11_err", (file_tc_error_test "error11.oat" (Failure "error11.oat:[2.18-2.19]: i is not declared.")));
    ("tc1_err", (file_tc_error_test "tc1.oat" (Failure "Constant is not of type int.")));
    ("tc2_err", (file_tc_error_test "tc2.oat" (Failure "tc2.oat:[1.11-1.19]: This expression has type int but an expression was expected of type bool.")));
    ("tc3_err", (file_tc_error_test "tc3.oat" (Failure "This expression has type int but an expression was expected of type bool.")));
    ("tc4_err", (file_tc_error_test "tc4.oat" (Failure "+ cannot take input type: (bool, int).")));
    ("tc5_err", (file_tc_error_test "tc5.oat" (Failure "* cannot take input type: (bool, bool).")));
    ("tc6_err", (file_tc_error_test "tc6.oat" (Failure "This expression has type int but an expression was expected of type bool.")));
    ("tc7_err", (file_tc_error_test "tc7.oat" (Failure "This expression has type bool but an expression was expected of type int.")));
    ("tc8_err", (file_tc_error_test "tc8.oat" (Failure "This expression has type bool but an expression was expected of type int[1].")));
    ("tc9_err", (file_tc_error_test "tc9.oat" (Failure "This expression has type int[] but an expression was expected of type int.")));
    ("tc10_err", (file_tc_error_test "tc10.oat" (Failure "! cannot take input type: int[3].")));
    ("tc11_err", (file_tc_error_test "tc11.oat" (Failure "This expression has type int but an expression was expected of type bool.")));
    ("tc12_err", (file_tc_error_test "tc12.oat" (Failure "Ecall cannot return unit.")));
    ("tc13_err", (file_tc_error_test "tc13.oat" (Failure "Scall must return unit.")));
    ("tc14_err", (file_tc_error_test "tc14.oat" (Failure "Constant cannot be of length 0.")));
    ("tc15_err", (file_tc_error_test "tc15.oat" (Failure "Constant cannot be of length 0.")));
    ("tc18_err", (file_tc_error_test "tc18.oat" (Failure "tc18.oat:[2.9-2.10]: f is not declared.")));
    ("tc19_err", (file_tc_error_test "tc19.oat" (Failure "tc19.oat:[3.9-3.10]: Ecall has wrong args.")));
    ("tc20_err", (file_tc_error_test "tc20.oat" (Failure "tc20.oat:[3.2-3.3]: Scall has wrong args.")));

  ]);

]

(*** End-to-end tests ***)
let file_tests : suite = [
  GradedTest("Easy tests", 20, [  
    ("run26", file_test "run26.oat" "" "0");
    ("run27", file_test "run27.oat" "" "99");
    ("run28", file_test "run28.oat" "" "18");
    ("run29", file_test "run29.oat" "" "1");
    ("run30", file_test "run30.oat" "" "9");
    ("run31", file_test "run31.oat" "" "9");
    ("run13", file_test "run13.oat" "" "1");
    ("run32", file_test "run32.oat" "" "34");
    ("run18", file_test "run18.oat" "" "-999");
    ("run19", file_test "run19.oat" "" "999");
    ("run20", file_test "run20.oat" "" "19");
    ("run21", file_test "run21.oat" "" "99");
    ("run24", file_test "run24.oat" "" "24");
    ("run33", file_test "run33.oat" "" "1");
    ("run34", file_test "run34.oat" "" "66");
    ("run35", file_test "run35.oat" "" "66");
    ("run38", file_test "run38.oat" "" "31");
    ("run39", file_test "run39.oat" "a" "2");
    ("run40", file_test "run40.oat" "" "8");
    ("run41", file_test "run41.oat" "" "3");
    ("run42", file_test "run42.oat" "" "2");
    ("run49", file_test "run49.oat" "" "abc0");
    ("run50", file_test "run50.oat" "" "abcde0");
    ("run51", file_test "run51.oat" "" "11");
    ("run52", file_test "run52.oat" "" "11");
  ]);

  GradedTest("Medium tests", 10, [
    ("run1", file_test "run1.oat" "" "153");
    ("run2", file_test "run2.oat" "" "6");
    ("run3", file_test "run3.oat" "" "2");
    ("run5", file_test "run5.oat" "" "4");
    ("run8", file_test "run8.oat" "" "2");
    ("run9", file_test "run9.oat" "" "4");
    ("run10", file_test "run10.oat" "" "5");
    ("run11", file_test "run11.oat" "" "7");
    ("run14", file_test "run14.oat" "" "16");
    ("run15", file_test "run15.oat" "" "19");
    ("run16", file_test "run16.oat" "" "13");
    ("run22", file_test "run22.oat" "" "abc0");
    ("run23", file_test "run23.oat" "" "1230");
    ("run25", file_test "run25.oat" "" "nnn0");
    ("run43", file_test "run43.oat" "" "42");
    ("run44", file_test "run44.oat" "" "hello0");
    ("run45", file_test "run45.oat" "" "420");
    ("run46", file_test "run46.oat" "" "420");
    ("run47", file_test "run47.oat" "" "3");
    ("run48", file_test "run48.oat" "" "11");
    ("lib1", file_test "lib1.oat" "" "3");
    ("lib2", file_test "lib2.oat" "" "2");
    ("lib3", file_test "lib3.oat" "" "4");
    ("lib4", file_test "lib4.oat" "" "532");
    ("lib5", file_test "lib5.oat" "" "532");
    ("lib6", file_test "lib6.oat" "" "565");
    ("lib7", file_test "lib7.oat" "" "565");
    ("lib8", file_test "lib8.oat" "" "Hello world!\n0");
    ("lib9", file_test "lib9.oat" "a b c d" "abcd5");
    ("lib10", file_test "lib10.oat" "" "6");
    ("lib11", file_test "lib11.oat" "" "45");
    ("lib14", file_test "lib14.oat" "" "~}|{zyxwvu0");
    ("lib15", file_test "lib15.oat" "123456789" "456780");
  ]);

  GradedTest("Hard tests", 10, [
    ("fac", file_test "fac.oat" "" "120");
    ("qsort", file_test "qsort.oat" "" "\nkpyf{shom\nfhkmopsy{\n255");
    ("bsort", file_test "bsort.oat" "" "y}xotnuw notuwxy}-1");
    ("msort", file_test "msort.oat" "" "~}|{zyxwvu uvwxyz{|}~ 0");
    ("msort2", file_test "msort2.oat" "" "~}|{zyxwvu uvwxyz{|}~ 0");
    ("hsort", file_test "hsort.oat" "" "0");
    ("stoogesort", file_test "stoogesort.oat" "" "-2435-63177-5759-3100");
    ("selectionsort", file_test "selectionsort.oat" "" "01253065992000");
    ("matrixmult", file_test "matrixmult.oat" "" 
       "19 16 13 23 \n5 6 7 6 \n19 16 13 23 \n5 6 7 6 \n0");

  ]);

  GradedTest("Runtime error tests", 20, [   
    ("run4", file_test ~ast2c:false "run4.oat" "" "Out-of-bound.");
    ("run6", file_test ~ast2c:false "run6.oat" "" "Out-of-bound.");
    ("run7", file_test ~ast2c:false "run7.oat" "" "Out-of-bound.");
    ("lib12", file_test ~ast2c:false "lib12.oat" "" "Out-of-bound.");
    ("run17", file_test ~ast2c:false "run17.oat" "" "Out-of-bound."); 
    ("run36", file_test ~ast2c:false "run36.oat" "" "Out-of-bound."); 
    ("run37", file_test ~ast2c:false "run37.oat" "" "Out-of-bound."); 
    ("lib15", file_test ~ast2c:false "lib15.oat" "" "Out-of-bound.");
  ]);
 
  GradedTest("Stress tests (hidden)", 10, [
  
  ]);
 
]

let manual_tests : suite = [
  GradedTest ("StyleManual", 10, [
  
  ]);
]

let graded_tests : suite = 
  
  typechecking_tests @
  file_tests @
  manual_tests
