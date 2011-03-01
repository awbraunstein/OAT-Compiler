open Il

type uid_list =
  | U of string * Il.uid list

let s = (stack : uid_list Stack.t) = Stack.create ()

type uids =
  | L of Il.uid list

let enter_scope (c: ctxt) : uid_list =

let lookup (s: string)(c: ctxt) : uid option =

let leave_scope (c: ctxt) : ctxt =
  begin match c with
    | h -> if h = s then return h else return None
    | [] -> return None
  end
  
let lookup (s: string)(c: ctxt) : uid option =
  begin match c with
    | h::tl -> if h = s then return h else return None
    | [] -> return None
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

