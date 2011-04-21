(* CIS341 main test harness *)
(* Author: Steve Zdancewic  *)

(* Do NOT modify this file -- we will overwrite it with our *)
(* own version when we test your homework.                  *)

open Assert
open Arg

let show_ast = ref false
let show_il = ref false
let ast2cpp = ref false
let il2c = ref false
let compile_only = ref false
let opt_ast = ref false 
let opt_il = ref false
let opt_asm = ref false


let lang_file_exn = ".oat"

let path_to_root_source (path:string) =
  (* The path is of the form ... "foo/bar/baz/<file>.e" *)
  let paths = Str.split (Str.regexp_string Platform.path_sep) path in
  let _ = if (List.length paths) = 0 then failwith "bad path" else () in
  let filename = List.hd (List.rev paths) in
  let len = String.length filename in
  let elen = String.length lang_file_exn in
  let exn = String.sub filename (len - elen) elen in
    if exn = lang_file_exn then 
      String.sub filename 0 (len - elen) 
    else
      failwith "Expected the file to end with .e"

let root_to_dot_i (root:string) =
  (!Platform.obj_path) ^ root ^ ".i"

let root_to_dot_s (root:string) =
  (!Platform.obj_path) ^ root ^ ".s"

let root_to_dot_o (root:string) =
  (!Platform.obj_path) ^ root ^ ".o"

let write_asm_cunit (cu:Cunit.cunit) (dot_s:string) : unit =
  let fout = open_out dot_s in
    Cunit.output_cunit cu fout;
    close_out fout

let do_one_file (path:string) : unit =
  let _ = Printf.printf "Processing: %s\n" path in
  let root = path_to_root_source path in
  let _ = if !Platform.verbose_on then Printf.printf "root name: %s\n" root 
    else () in

  try
  (* Preprocess the file *)
  let dot_i = root_to_dot_i root in
  Platform.preprocess path dot_i;

  (* Parse the file *)
  let buffer = open_in dot_i in
  let prog = Phase1.parse (root ^ lang_file_exn) (Lexing.from_channel buffer) in
  let _ = close_in buffer in
  let _ = if !show_ast then Astlib.print_prog prog else () in
  let _ = 
    if !ast2cpp then
      let result = Astlib.cpp_run_prog prog "" in
      Printf.printf "AST2CPP Program output %s\n" result  
    else () 
  in
  

  (* Typechecking *)
  (try
    Tc.typecheck_prog prog
  with  
| Failure f -> Printf.printf "Failed: %s" f
  );
  (* Optimize the ast *)
  let prog = if !opt_ast then Optast.opt_ast prog else prog in

  (* Translate to IL form *)
  let prog_il = Phase1.compile_prog prog in

  (* Optimize the il *)
  let prog_il = if !opt_il then Optil.opt_il prog_il else prog_il in

  let _ = if !show_il then Printf.printf "%s\n" (Il.string_of_prog prog_il) in
  let _ = 
    if !il2c then
      let result = Il.c_run_prog prog_il "" in
      Printf.printf "IL2C Program output %s\n" result  
    else () 
  in

  (* Compile it to a .s file *)
  let dot_s = root_to_dot_s root in
  let cu = Phase2.compile_prog prog_il in

  (* Optimize the asm *)
  let cu = if !opt_asm then Optasm.opt_asm cu else cu in
  let _ = write_asm_cunit cu dot_s in

  if (!compile_only) then () else
    (* Assemble it to a .o file *)
    let dot_o = root_to_dot_o root in
    Platform.assemble dot_s dot_o
  with
| Failure f -> Printf.printf "Failed: %s" f 
| Lexer.Lexer_error (r,m) -> Printf.printf "Lexing error: %s %s\n" (Range.string_of_range r) m


let parse_stdin (arg:int) =
  let rec loop (i:int) = 
    let st = read_line () in 
    begin try
      let prog = Phase1.parse "stdin" (Lexing.from_string st) in
      Astlib.print_prog prog; ()
    with
| Failure f -> Printf.printf "Failed: %s" f 
| Lexer.Lexer_error (r,m) -> Printf.printf "Lexing error: %s %s\n" (Range.string_of_range r) m
   
    end; loop (i+1)
  in
    loop 0



exception Ran_tests

let worklist = ref []

let suite = ref (Providedtests.provided_tests @ 
                 
                 Gradedtests.graded_tests)

let exec_tests () =
  let o = run_suite !suite in
  Printf.printf "%s\n" (outcome_to_string o);
  raise Ran_tests




(* Use the --test option to run unit tests and the quit the program. *)
let argspec = [
  ("--test", Unit exec_tests, "run the test suite, ignoring earlier inputs");
  ("-q", Clear Platform.verbose_on, "quiet mode -- turn off verbose output");
  ("-bin-path", Set_string Platform.bin_path, "set the output path for generated executable files, default c_bin");
  ("-obj-path", Set_string Platform.obj_path, "set the output path for generated .s  and .o files, default c_obj");
  ("-test-path", Set_string Gradedtests.test_path, "set the path to the test directory");
  ("-lib", String (fun p -> Platform.lib_paths := (p::(!Platform.lib_paths))), "add a library to the linked files");
  ("-runtime", Set_string Platform.runtime_path, "set the .c file to be used as the language runtime implementation");
  ("-l", String (fun p -> Platform.libs := (p::(!Platform.libs))), "add a library to the linked files");
  ("-I", String (fun p -> Platform.include_paths := (p::(!Platform.include_paths))), "add <directory> to the compiler's search paths");
  ("-o", Set_string Platform.executable_name, "set the output executable's name");
  ("-S", Set compile_only, "compile only, creating .s files from the source");
  ("-Oast", Set opt_ast, "optimize at the ast level");
  ("-Oil", Set opt_il, "optimize at the il level");
  ("-Oasm", Set opt_asm, "optimize at the asm level");
  ("-linux", Set Platform.linux, "use linux-style name mangling");
  ("--stdin", Int parse_stdin, "parse and interpret inputs from the command line, where X = <arg>");
  ("--clean", Unit (Platform.clean_paths), "clean the output directories, removing all files");
  ("--show-ast", Set show_ast, "print the abstract syntax tree after parsing");
  ("--show-il", Set show_il, "print the il representation");
  ("--ast2cpp", Set ast2cpp, "print and execute the CPP code from the abstract syntax tree");
  ("--il2c", Set il2c, "print and execute the C code from the il representation");

]

let _ =
  try
    Arg.parse argspec (fun f -> worklist := f :: !worklist)
        "CIS341 main test harness \n";
    match !worklist with
    | [] -> print_endline "* Nothing to do"
    | _ -> (* assemble the files *)
	   List.iter do_one_file !worklist;
	   (* link the files if necessary *)
	   if !compile_only then () 
	   else
	     let dot_o_files = List.map (fun p -> root_to_dot_o (path_to_root_source p)) !worklist in
	       Platform.link dot_o_files (Platform.full_executable_name ())
  with Ran_tests -> ()


