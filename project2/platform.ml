(** Assembling and linking for X86.  Depends on the underlying OS platform    *)
(******************************************************************************)

open Printf
open Unix

exception AsmLinkError of string * string

(****************************************************************)
(* Platform specific configuration: Unix/Max vs. Windows/Cygwin *)
(****************************************************************)
let os = Sys.os_type   (* One of "Unix" "Win32" or "Cygwin" *)
let runtime_path = ref "runtime.c"
let lib_paths = ref []
let obj_path = ref (if os = "Unix" then "c_obj/" else "c_obj\\")
let bin_path = ref (if os = "Unix" then "c_bin/" else "c_bin\\")
let executable_name = ref ((!bin_path) ^ (if os = "Unix" then "a.out" else "a.exe"))
let executable_exn = if os = "Unix" then "" else ".exe" 
let path_sep = if (os = "Unix") then "/" else "\\"
let linux = ref false

let as_cmd = if os = "Unix" then "gcc -c -m32 -o " else "as -g -o "
let link_cmd = if os = "Unix" then "gcc -m32 -o " else 
  "gcc-4 -m32 -o "
let rm_cmd = if os = "Unix" then "rm -f " else "del /Q "
let dot_path = if os = "Unix" then "./" else ".\\"
(****************************************************************)


let verbose_on = ref true

let decorate_cdecl name = if !linux then name else ("_" ^ name)

let sh (cmd:string) : unit =
  (if !verbose_on then (printf "* %s" cmd; print_newline ()));
  match (system cmd) with
  | WEXITED i when i <> 0 ->
      raise (AsmLinkError (cmd, sprintf "Stopped with %d." i))
  | WSIGNALED i -> raise (AsmLinkError (cmd, sprintf "Signaled with %d." i))
  | WSTOPPED i -> raise (AsmLinkError (cmd, sprintf "Stopped with %d." i))
  | _ -> ()

let gen_name (basedir:string) (basen:string) (baseext:string) : string =  
  let rec nocollide ofs =
    let ctime = Int64.add (Int64.of_float (time ())) ofs in
    let nfn = sprintf "%s%s%Lx%s" basedir basen ctime baseext in
      try ignore (stat nfn); nocollide (Int64.add ofs 1L)
      with Unix_error (ENOENT,_,_) -> nfn
  in nocollide 0L
  
let assemble (dot_s:string) (dot_o:string) : unit =
  sh (sprintf "%s%s %s" as_cmd dot_o dot_s)

let link (mods:string list) (out_fn:string) : unit =
  sh (sprintf "%s%s %s" link_cmd out_fn 
	(String.concat " " (mods @ (!runtime_path :: !lib_paths))))

let run_executable arg pr =
  let cmd = sprintf "%s%s %d" dot_path pr arg in
  (if !verbose_on then printf "* %s\n" cmd);
  match (system cmd) with
  | WEXITED i -> i
  | WSIGNALED i -> raise (AsmLinkError (cmd, sprintf "Signaled with %d." i))
  | WSTOPPED i -> raise (AsmLinkError (cmd, sprintf "Stopped with %d." i))

let run_executable_to_tmpfile arg pr tmp =
  let cmd = sprintf "%s%s %d > %s" dot_path pr arg tmp in
  (if !verbose_on then printf "* %s\n" cmd);
  match (system cmd) with
  | WEXITED i -> ()
  | WSIGNALED i -> raise (AsmLinkError (cmd, sprintf "Signaled with %d." i))
  | WSTOPPED i -> raise (AsmLinkError (cmd, sprintf "Stopped with %d." i))

let clean_paths () =
  sh (sprintf "%s%s*" rm_cmd !obj_path);
  sh (sprintf "%s%s*" rm_cmd !bin_path)
