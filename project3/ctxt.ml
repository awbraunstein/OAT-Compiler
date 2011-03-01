open Il

type ctxt = {
  ctxt_stack : (string * Il.uid list) list;
  ctxt_uids : Il.uid list;
  ctxt_set : Il.uid list;
}

let enter_scope (c: ctxt) : ctxt =
  begin match c.ctxt_stack with
    | h::tl -> tl
    | [] -> c.ctxt_stack
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

