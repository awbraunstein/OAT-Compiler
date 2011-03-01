open Il

type ctxt = {
  mutable ctxt_stack : ((string * Il.uid) list) list;
  mutable ctxt_uids : Il.uid list;
  mutable ctxt_set : Il.uid list;
}

let find_helper (l:(string * Il.uid) list) (s: string) : uid =
  begin match l with
    | h::tl ->
      begin match h with
        | (x,y) -> if x = s then y
      end
   end
  
let enter_scope (c: ctxt) : ctxt =
  let return_c : ctxt = c in
  begin match c.ctxt_stack with
    | h::tl -> return_c.ctxt_stack <- c.ctxt_stack @ return_c.ctxt_stack; return_c
    | [] -> return_c
  end
  
let leave_scope (c: ctxt) : ctxt =
 let return_c : ctxt = c in
  begin match c.ctxt_stack with
    | h::tl -> return_c.ctxt_stack <- tl; return_c
    | [] -> return_c.ctxt_stack <- return_c.ctxt_stack; return_c
  end

let lookup (s:string) (c:ctxt) : uid option =
  find_helper ctxt_stack s
          

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

