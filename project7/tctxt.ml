open Ast

type ctxt = {
  this   : cid option;  (* This class *)
  vdecls : contexts;    (* Contexts of globals, args and locals *)
  fdecls : dcontext;    (* Function declararion contexts *)
  sigs   : signature;   (* Class signature *)
  }

let print_sigs sigs =
  List.iter (fun (cid, (extopt, fs, ts, ms)) ->
      Printf.printf "%s " cid;
      (match extopt with 
	| None -> ()
	| Some pid -> Printf.printf "<: %s" pid);
      Printf.printf "\nfs= ";
      List.iter (fun (id, t) -> 
        Printf.printf "(%s, %s)" id (Astlib.string_of_typ t)) fs;
      Printf.printf "\nts= ";
      List.iter (fun t -> Printf.printf "%s " (Astlib.string_of_typ t)) ts;
      Printf.printf "\nms= ";
      List.iter (fun (id, ft) -> 
        Printf.printf "(%s, %s)" id (Astlib.string_of_ftyp ft)) ms;
      Printf.printf "\n"
    ) sigs

(* Programs always have an Object class:
 * class Object { 
 *   string _name;
 *   new()() 
 *     this._name = "Object";
 *   {}
 *   string get_name () {
 *     return this._name;
 *   }
 * };   
*)
let empty_ctxt = {this = None; vdecls = []; fdecls = []; 
  sigs = [("Object", (None, 
                      [("_name", TRef RString)], [],
                      [("get_name", ([], Some (TRef RString)))]))]}

let enter_scope c =
  {c with vdecls = []::c.vdecls}

let leave_scope c =
  {c with vdecls = 
    match c.vdecls with
      | [] -> failwith "Internal error: leave_scope called in no scope."
      | _::cs -> cs}

exception Found of typ

let lookup_vdecl s c =
  try 
    List.iter 
      (fun c -> 
         try 
           raise (Found (List.assoc s c))
         with
           Not_found -> ()
      ) c.vdecls;
    raise Not_found;
  with
    | Not_found -> None
    | Found t -> Some t

let lookup_builtin_fdecl (fid:string) : ftyp option =
  begin match fid with
  | "string_of_array" -> Some ([TRef (RArray TInt)], Some (TRef RString))
  | "array_of_string" -> Some ([TRef RString], Some (TRef (RArray TInt)))
  | "print_string" -> Some ([TRef RString], None)
  | "print_int" ->  Some ([TInt], None)
  | "print_bool" -> Some ([TBool], None)
  | "alloc_array" -> Some ([TInt], Some (TRef (RArray TInt)))
  | "string_cat" -> Some ([TRef RString; TRef RString], Some (TRef RString))
  | "string_alloc" -> Some ([TInt], Some (TRef RString))
  | "length_of_string" -> Some ([TRef RString], Some TInt)
  | "string_of_int" -> Some ([TInt], Some (TRef RString))
  | "int_of_string" -> Some ([TRef RString], Some TInt)
  | "string_of_bool" -> Some ([TBool], Some (TRef RString))
  | "bool_of_string" -> Some ([TRef RString], Some TBool)
  | "string_at" -> Some ([TRef RString; TInt], Some TInt)
  | "string_set" -> Some ([TRef RString; TInt; TInt], None)
  | "write_file" -> Some ([TRef RString; TRef RString], Some TInt)
  | "read_file" -> Some ([TRef RString], Some (TRef RString))
  | "random_int" -> Some ([], Some TInt)
  | "oat_abort" -> Some ([TInt], None)
  |  _ -> None
  end

let lookup_fdecl s c =
  try 
    Some (List.assoc s c.fdecls)
  with
    | Not_found -> lookup_builtin_fdecl s

let lookup_sig s c =
  try 
    Some (List.assoc s c.sigs)
  with
    | Not_found -> None

let rec hasField sigs cid id =
  match sigs with
    | [] -> None
    | (cid', (Some pid, fs, _, _))::sigs' -> 
        if (cid = cid') then 
          try Some (List.assoc id fs) 
          with | Not_found _ -> hasField sigs' pid id
        else hasField sigs' cid id   
    | (cid', (None, fs, _, _))::sigs' -> 
        if (cid = cid') then 
          try Some (List.assoc id fs) with | Not_found _ -> None
        else hasField sigs' cid id   

let rec hasMethod sigs cid id =
  match sigs with
    | [] -> None
    | (cid', (Some pid, _, _, ms))::sigs' -> 
        if (cid = cid') then 
          try Some (List.assoc id ms) 
          with | Not_found _ -> hasMethod sigs' pid id
        else hasMethod sigs' cid id   
    | (cid', (None, _, _, ms))::sigs' -> 
        if (cid = cid') then 
          try Some (List.assoc id ms) with | Not_found _ -> None
        else hasMethod sigs' cid id   

let add_vdecl s t c =
  {c with vdecls = 
  match c.vdecls with
    | [] -> failwith "Internal error: add_vdecl called in no scope."
    | lc::cs' -> 
        try
          ignore (List.assoc s lc);
          failwith ("redeclaraion of a local variable: " ^ s)
        with
          | Not_found -> 
              try
                ignore (List.assoc s c.fdecls);
                failwith ("a function and a variable have the same name: " ^ s)
              with
                | Not_found -> ((s, t)::lc)::cs'} 

let add_fdecl s ft c =
  match (lookup_vdecl s c) with
    | Some _ -> failwith ("a function and a variable have the same name: " ^ s)
    | None ->
  try
    ignore (List.assoc s c.fdecls);
    failwith ("redeclaraion of a function: " ^ s)
  with
    | Not_found -> {c with fdecls = (s, ft)::c.fdecls}

let add_sig cid extop ts c =
  try 
    ignore (List.assoc cid c.sigs);
    failwith ("redeclaration of a class: " ^ cid)
  with
    | Not_found ->
        match extop with
          | Some pid ->
              begin try 
                ignore (List.assoc pid c.sigs)
              with
                Not_found ->
                  failwith ("parent class is not defined: " ^ pid)
              end;
              {c with sigs = (cid, (extop, [], ts, []))::c.sigs}
          | None ->
              {c with sigs = (cid, (Some "Object", [], ts, []))::c.sigs}


