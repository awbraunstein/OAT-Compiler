open Il

type ctxt = {
  ctxt_stack : ((string * Il.uid) list) list;
  ctxt_uids : Il.uid list;
  ctxt_set : Il.uid list;
}

let mk_ctxt : ctxt = 
  {ctxt_stack = [];ctxt_uids = []; ctxt_set = []}

let enter_scope (c: ctxt) : ctxt =
  begin match c with
    | {ctxt_stack = x; ctxt_uids = y; ctxt_set = z;} ->
      {ctxt_stack = [] @ x; ctxt_uids = y; ctxt_set = z;}
  end
  
let leave_scope (c: ctxt) : ctxt =
  begin match c with
    | {ctxt_stack = x; ctxt_uids = y; ctxt_set = z;} ->
      begin match x with
        | h::tl -> {ctxt_stack = tl; ctxt_uids = y; ctxt_set = z;}
        | [] -> {ctxt_stack = []; ctxt_uids = y; ctxt_set = z;}
      end
  end


let alloc (s: string) (c: ctxt) : ctxt * uid =
  let u = mk_uid s in
  begin match c with
    | {ctxt_stack = x; ctxt_uids = y; ctxt_set = z;} ->
      begin match x with
        | h::tl -> begin try ignore (List.assoc s h); failwith "already alloc'd" with Not_found ->
            ({ctxt_stack = ([(s,u)] @ h)::tl; ctxt_uids = y @ [u]; ctxt_set = z;}, u)
          end
        | [] -> ({ctxt_stack = [[(s,u)]]; ctxt_uids = y @ [u]; ctxt_set = z;},u)
      end
  end

        
let lookup (s:string)(c:ctxt) : uid option =
  begin match c with 
   | {ctxt_stack = x; ctxt_uids = y; ctxt_set = z;} ->
    let rec lookup_r (l:uid list):uid option  =
      begin match l with
        | h::tl -> begin try Some (List.assoc s h) with Not_found -> lookup_r tl end
        | [] -> None
      end in lookup_r x
  end

(* One possible implementation of a context is:
   - a stack of association (string, uid) lists to implement scoping
   - a set of available slots (possibly recycled from elsewhere)
   - a list of all slots (i.e. uids) ever used 
 
   A suggested implementation of contexts supports these operations:
   - enter_scope : ctxt -> ctxt
   - alloc : string -> ctxt -> (ctxt * uid)
   - leave_scope : ctxt -> ctxt
   - lookup : string -> ctxt -> uid option

   However, you are free to use any datastructure or interface that you choose.
*)

