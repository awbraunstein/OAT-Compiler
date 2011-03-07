open Il

type ctxt = {
  stack : ((string * uid) list) list;
  avail : uid list;
  uids : uid list;
  globals : (string * Cunit.global_data) list; 
  fdecls : (string * X86.lbl) list;
  args : (string * int) list;
}

let empty_ctxt = {stack = [];
		  avail = [];
		  uids = [];
                  globals = [];
                  fdecls = [];
                  args = [];}

let enter_scope c =
  {c with stack = []::c.stack}

let leave_scope c =
  match c.stack with
  | [] -> failwith "leave_scope called in no scope."
  | frame::stack ->
      {stack = stack;
       avail = (List.map snd frame)@c.avail;
       uids = c.uids;
       globals = c.globals;
       fdecls = c.fdecls;
       args = c.args;}

let alloc s c =
  match c.stack with
  | [] -> failwith "alloc called in no scope."
  | frame::stack ->
      try
        ignore (List.assoc s c.fdecls);
        failwith "a function and a local have the same name"
      with
        | Not_found ->
      try 
        ignore (List.assoc s frame); 
        failwith "redeclaraion of a local variable" 
      with
	| Not_found ->
      (match c.avail with 
       | [] -> (let v = mk_uid s in
		  ({stack = ((s,v)::frame)::stack;
		   avail = [];
		   uids = v::c.uids;
                   globals = c.globals;
                   fdecls = c.fdecls;
                   args = c.args;}, v))
       | v::rest ->({stack = ((s,v)::frame)::stack;
		     avail = rest;
		     uids = c.uids;
                     globals = c.globals;
                     fdecls = c.fdecls;
                     args = c.args;}, v))

let add_global_data so c hint v =
  try
    match so with
      | Some s ->
        ignore (List.assoc s c.fdecls);
        failwith "a function and a global have the same name"
      | _ -> raise Not_found
  with
    | Not_found ->
  try
    match so with
      | Some s ->
         ignore (List.assoc s c.globals);
         failwith "redeclaraion of a global variable"
      | _ -> raise Not_found
  with
    | Not_found ->
      let l = X86.mk_lbl_hint hint in
      let k = match so with Some s -> s | None -> X86.string_of_lbl l in
      let g = {Cunit.link=false;
               Cunit.label=l;
               Cunit.value=v}
      in
      ({c with globals = (k, g)::c.globals}, g)

let add_global_int32 so i32 c =
  add_global_data so c "gint32" (Cunit.GInt32 i32) 

let add_fdecl s c =
  try
    ignore (List.assoc s c.globals);
    failwith "a function and a global have the same name"
  with
    | Not_found ->
  try
    ignore (List.assoc s c.fdecls);
    failwith "redeclaraion of a function"
  with
    | Not_found ->
      let l = X86.mk_lbl_hint s in 
      ({c with fdecls = (s, l)::c.fdecls}, l)
 
let add_args s c =
  try
    ignore (List.assoc s c.fdecls);
    failwith "a function and an argument have the same name"
  with
    | Not_found ->
  try
    ignore (List.assoc s c.args);
    failwith "redeclaraion of an argument"
  with
    | Not_found ->
      let i = List.length c.args in 
      ({c with args = (s, i)::c.args}, i)

let clear_args c = {c with args = []}
 
exception Found of operand

let lookup_stack s c =
  let lookup_f frame = 
    try raise (Found (Slot (List.assoc s frame))) with
      | Not_found -> ()
  in
    try (List.iter lookup_f c.stack);None with
      | Found op -> Some op

let lookup_builtin_fdecl (fid:string) : X86.lbl option =
  if (fid = "length_of_array" ||
      fid = "array_of_string" ||
      fid = "string_of_array" ||
      fid = "alloc_array" ||
      fid = "print_string" ||
      fid = "print_int" ||
      fid = "print_bool")
  then Some (X86.mk_lbl_named fid)
  else None

let lookup_fdecl s c =
  try 
    List.assoc s c.fdecls
  with
    | Not_found -> 
      match (lookup_builtin_fdecl s) with
	| Some n -> n
	| None -> failwith (Printf.sprintf "Fdecl %s is not defined." s) 

let lookup_args s c =
  try 
    try raise (Found (Arg (List.assoc s c.args))) with
      | Not_found -> None
  with    
    | Found op -> Some op

let lookup_globals s c =
  try 
    try raise (Found (Global (List.assoc s c.globals))) with
      | Not_found -> None
  with    
    | Found op -> Some op

let lookup s c =
  match lookup_stack s c with
    | Some op -> Some op
    | None -> 
      match lookup_args s c with
	| Some op -> Some op
        | None -> lookup_globals s c

let size c = List.length c.uids


