open Ast

type ctxt = {
  vdecls : Range.t contexts;             (* Variable contexts *)
  fdecls : Range.t dcontext;             (* Function declararion contexts *)
  }

let empty_ctxt = {vdecls = []; fdecls = [];}

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
  | "string_of_array" -> Some ([TArray TInt], Some TString)
  | "array_of_string" -> Some ([TString], Some (TArray TInt))
  | "print_string" -> Some ([TString], None)
  | "print_int" ->  Some ([TInt], None)
  | "print_bool" -> Some ([TBool], None)
  | "alloc_array" -> Some ([TInt], Some (TArray TInt))
  |  _ -> None
  end

let lookup_fdecl s c =
  try 
    Some (List.assoc s c.fdecls)
  with
    | Not_found -> lookup_builtin_fdecl s

let add_vdecl s t c =
  {c with vdecls = 
  match c.vdecls with
    | [] -> failwith "Internal error: add_vdecl called in no scope."
    | lc::cs' -> 
        try
          ignore (List.assoc s lc);
          failwith "redeclaraion of a local variable"
        with
          | Not_found -> 
              try
                ignore (List.assoc s c.fdecls);
                failwith "a function and a variable have the same name"
              with
                | Not_found -> ((s, t)::lc)::cs'} 

let add_fdecl s ft c =
  match (lookup_vdecl s c) with
    | Some _ -> failwith "a function and a variable have the same name"
    | None ->
  try
    ignore (List.assoc s c.fdecls);
    failwith "redeclaraion of a function"
  with
    | Not_found -> {c with fdecls = (s, ft)::c.fdecls}

let add_fdecls (c:ctxt) (p:Range.t prog) : ctxt =
  List.fold_left
   (fun c -> fun g ->
      match g with
  	| Gfdecl (rt, (_,fid), args, _, _) ->
            let (ts, _) = List.split args in
            add_fdecl fid (ts, rt) c
  	| _ -> c
   ) c p