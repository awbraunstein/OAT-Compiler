open Il
open Printf

(* Method layout:
 * ----------------
 * entry:
 *   prologue
 *   jump first_lbl
 * first_lbl:
 *   ...
 *   epilogue
 * ----------------
*)
type cmeth = {
  cm_offset: int;              (* offset *)
  cm_entry: X86.lbl;           (* the entry of the method *)
  cm_first_lbl: X86.lbl;       (* the first label after the prologue *)
  cm_ftyp: Ast.ftyp;           (* the type of the method *)
}

(* Class representation:
 * ---------------------------
 * entry:
 *   dispatch_lbl of super class
 * dispatch_lbl:
 *   m1      // offset = 0
 *   m2      // offset = 1
 *   ...     // ...
 * ---------------------------
 *
 * A value of class:
 *         -------+
 *                v 
 * ------------------------------------------
 * | dispatch_lbl |   f0   |   f1   | ...
 * ------------------------------------------
 *                 offset=0 offset=1 ... 
*)
type cdecl = {
  cd_entry: X86.lbl;                          (* the label of this class *)
  cd_dispatch_lbl: X86.lbl;               (* the label of dispatch table *)
  cd_super: string option;                    (* the name of super class *)
  cd_fields: (string * (int * Ast.typ)) list;   (* name -> offset and typ*)
  cd_methods: (string * cmeth) list;                   (* name -> method *)
  }

(* Context also tracks type informations of locals, args, globals, and class 
 * members to compile field or method accesses. 
*)
type ctxt = {
  avail : uid list;
  uids : uid list;
  args : (string * (int * Ast.typ)) list;
  stack : ((string * (uid * Ast.typ option)) list) list;
  globals : (string * (Cunit.global_data * Ast.typ)) list; 
  fdecls : (string * (X86.lbl * Ast.ftyp)) list; (* fun name -> entry and typ *)
  cdecls : (string * cdecl) list;
  cthis : string option;                         (* This class name *)
  }

let empty_ctxt = {stack = []; avail = []; uids = []; globals = []; 
                  fdecls = []; args = []; cdecls = []; cthis = None}

let size c = List.length c.uids

let enter_scope c =
  {c with stack = []::c.stack}

let leave_scope c =
  match c.stack with
  | [] -> failwith "leave_scope called in no scope."
  | frame::stack ->
      {c with stack = stack; 
              avail = (List.map (fun x -> fst (snd x)) frame)@c.avail}

let add_args s t c =
  try
    ignore (List.assoc s c.fdecls);
    failwith ("a function and an argument have the same name: " ^ s)
  with
    | Not_found ->
  try
    ignore (List.assoc s c.args);
    failwith ("redeclaraion of an argument: " ^ s)
  with
    | Not_found ->
      let i = List.length c.args in 
      {c with args = (s,(i,t))::c.args}

let clear_args c = (List.length c.args, {c with args = []})
 
(* If we ensure that the allocated slot is not used for any class type,
 * we can simply give its type [top] to be None. For such a temparory variable,
 * we cannot access its fields or methods if it is really a class variable.
*)
let alloc s top c =
  match c.stack with
  | [] -> failwith "alloc called in no scope."
  | frame::stack ->
      try
        ignore (List.assoc s c.fdecls);
        failwith ("a function and a local have the same name: " ^ s)
      with
        | Not_found ->
      try 
        ignore (List.assoc s frame); 
        failwith ("redeclaraion of a local variable: " ^ s) 
      with
	| Not_found ->
      (match c.avail with 
       | [] -> let v = mk_uid s in
           ({c with stack = ((s,(v,top))::frame)::stack; avail = []; 
                    uids = v::c.uids;}, v)
       | v::rest ->
           ({c with stack = ((s,(v,top))::frame)::stack; avail = rest;}, v))

let add_global_data so v t hint c =
  try
    match so with
      | Some s ->
        ignore (List.assoc s c.fdecls);
        failwith ("a function and a global have the same name: " ^ s)
      | _ -> raise Not_found
  with
    | Not_found ->
  try
    match so with
      | Some s ->
         ignore (List.assoc s c.globals);
         failwith ("redeclaraion of a global variable: " ^ s)
      | _ -> raise Not_found
  with
    | Not_found ->
      let l = X86.mk_lbl_hint hint in
      let k = match so with Some s -> s | None -> X86.string_of_lbl l in
      let g = {Cunit.link=false;
               Cunit.label=l;
               Cunit.value=v}
      in
      {c with globals = (k,(g,t))::c.globals}

let add_global_int32 so i32 t c =
  add_global_data so (Cunit.GInt32 i32) t "gint32" c  

let add_fdecl s ft c =
  try
    ignore (List.assoc s c.globals);
    failwith ("a function and a global have the same name: " ^ s)
  with
    | Not_found ->
  try
    ignore (List.assoc s c.fdecls);
    failwith ("redeclaraion of a function: " ^ s)
  with
    | Not_found ->
      let l = X86.mk_lbl_hint s in 
      {c with fdecls = (s, (l, ft))::c.fdecls}

(* generate names for class methods and constructors. *) 
let mk_class_lbl cid mid =
  X86.mk_lbl_hint (cid ^ "_" ^mid) 

let mk_ctor_name cid = 
  "_" ^ cid ^ "_ctor"

(* create a new [cdecl] of class [cid] with parent class [super_opt] in the 
 * context [c]. Fields and methods are set to be empty. 
*)
let add_cdecl c cid super_opt =
  let lthis = X86.mk_lbl_hint cid in
  let lvtbl = X86.mk_lbl_hint (cid ^ "_vtable") in
  let super_opt = (match super_opt with
    | None -> if cid <> "Object" then Some "Object" else None
    | Some _ -> super_opt) in
  match super_opt with
    | None -> 
        {c with cthis=Some cid; cdecls=(cid, 
          {cd_entry=lthis;cd_dispatch_lbl=lvtbl; 
           cd_super=None;cd_fields=[];cd_methods=[]})::c.cdecls}
    | Some super ->
        try
          let cd = List.assoc super c.cdecls in
          {c with cthis=Some cid; cdecls=(cid, 
            {cd_entry=lthis;cd_dispatch_lbl=lvtbl;cd_super=super_opt;
             cd_fields=cd.cd_fields;cd_methods=cd.cd_methods})::c.cdecls}
        with
          | Not_found -> 
              failwith (sprintf "Super %s is not defined." super) 

(* Update this class id when entering or leaving a class scope. *)
let set_this c cid = {c with cthis=Some cid}
let unset_this c = {c with cthis=None}

(* Update the class [cid]'s declarations to be [cd] in the cdecl list [cds]. *)
let update_cdecls cid cd cds =
  let rec _update_cdecls cid cd accum cds =
  match cds with
    | [] -> failwith (sprintf "Class %s is not defined." cid)
    | (cid',_) as h::cds' -> 
        if cid = cid' then accum@(cid,cd)::cds'
        else _update_cdecls cid cd (h::accum) cds'
  in _update_cdecls cid cd [] cds

let add_cfield cid fid t c =
  try 
    let cd = List.assoc cid c.cdecls in
    try
      ignore (List.assoc fid cd.cd_methods);
      failwith ("add_cfield: a method and a variable have the same name: " ^ fid)
    with
      | Not_found ->
          try
            ignore (List.assoc fid cd.cd_fields);
            failwith ("redeclaraion of a class field: " ^ fid)
          with
           | Not_found -> 
               let i = List.length cd.cd_fields in 
               {c with cdecls = update_cdecls cid 
                 {cd with cd_fields=(fid, (i, t))::cd.cd_fields} c.cdecls}    
  with
    | Not_found -> failwith (sprintf "Class %s is not defined." cid)

(* Overload the class [cid]'s method [mid] with typ [ft], and update the
 * method list [ms]. 
*)
let rec overload_cmethod cid mid ft ms =
  match ms with
    | [] -> raise Not_found 
    | (mid', cm) as h::tl ->
      if mid' = mid then
        (mid, {cm with cm_entry=mk_class_lbl cid mid;
                       cm_first_lbl=mk_class_lbl cid mid;
                       cm_ftyp=ft})::tl
      else
        let tl' = overload_cmethod cid mid ft tl in
        h::tl'

let add_cmethod cid mid ft c =
  try 
    let cd = List.assoc cid c.cdecls in
    try
      ignore (List.assoc mid cd.cd_fields);
      failwith ("add_cmethod: a method and a variable have the same name: "^mid)
    with
      | Not_found -> let ms = 
          try overload_cmethod cid mid ft cd.cd_methods
          with
           | Not_found -> 
               (mid, {cm_offset=List.length cd.cd_methods;
                      cm_entry=mk_class_lbl cid mid;
                      cm_first_lbl=mk_class_lbl cid mid;
                      cm_ftyp=ft})::cd.cd_methods in
         {c with cdecls = update_cdecls cid {cd with cd_methods=ms} c.cdecls}    
  with
    | Not_found -> failwith (sprintf "Class %s is not defined." cid)

exception Found of (operand * Ast.typ option)

let lookup_stack s c =
  let lookup_f frame = 
    try
      let (u, top) = List.assoc s frame in
      raise (Found (Slot u, top)) 
    with
      | Not_found -> ()
  in
    try (List.iter lookup_f c.stack);None with
      | Found op -> Some op

let lookup_builtin_fdecl (fid:string) : (X86.lbl * Ast.ftyp) option =
  match Tctxt.lookup_builtin_fdecl fid with
    | Some ftyp -> Some (X86.mk_lbl_named fid, ftyp)
    | None ->
       if fid = "length_of_array" then
          Some (X86.mk_lbl_named fid, ([], Some Ast.TInt))
       else None

let lookup_fdecl s c =
  try 
    List.assoc s c.fdecls
  with
    | Not_found -> 
      match (lookup_builtin_fdecl s) with
	| Some n -> n
	| None -> failwith (sprintf "Fdecl %s is not defined." s) 

let lookup_arg s c =
  try 
    let (i, t) = List.assoc s c.args in
    Some (Arg i, Some t) 
  with
    | Not_found -> None

let lookup_global s c =
  try 
    let (g, t) = List.assoc s c.globals in
    Some (Global g, Some t)
  with
    | Not_found -> None

let lookup s c =
  match lookup_stack s c with
    | Some op -> Some op
    | None -> 
      match lookup_arg s c with
	| Some op -> Some op
        | None -> lookup_global s c

let lookup_cdecl cid c =
  try
    List.assoc cid c.cdecls
  with
    | Not_found -> failwith (sprintf "Class %s is not defined." cid) 

let lookup_cmethod cid mid c =
  let cd = lookup_cdecl cid c in
  try 
    Some (List.assoc mid cd.cd_methods)
  with
    | Not_found -> None

let lookup_cfield cid fid c =
  let cd = lookup_cdecl cid c in
  try 
    Some (List.assoc fid cd.cd_fields) 
  with
    | Not_found -> None

(* Some helper functions for this. *)
let check_this c =
  match c.cthis with
    | None -> failwith "This is not in class."
    | Some cid -> cid

let lookup_this_cmethod s c =
  lookup_cmethod (check_this c) s c

let lookup_this_cfield s c =
  lookup_cfield (check_this c) s c

let lookup_this_cfields c =
  (lookup_cdecl (check_this c) c).cd_fields

let lookup_this_class_entry c =
  (lookup_cdecl (check_this c) c).cd_entry

let lookup_super_class c =
  let cid = check_this c in
  try
    (List.assoc cid c.cdecls).cd_super
  with
    | Not_found -> failwith (sprintf "Class %s is not defined." cid) 

