
(* Opening Modules 
 * 
 * When opening modules with 'open' it's better to do a
 * 'local' open rather than a global one:   *)
let average x y =
    let open Int64 in
    x + y / of_int 2;;

(*   or....   *)

let average x y =
    Int64.(x + y / of_int 2);;

(*   or....   *)

let average x y =
    let module I = Int64 in
    I.(x + y / of_int 2);;


(* Including Modules  
 * 
 * Opening a module affects the environment used to search for identifiers.
 * 'including' a module is a way of actually adding new identifiers to a module.
 *  For example if we define an Interval module:         *)

module Interval = struct
    type t = |Interval of int * int
             |Empty
let create low high =
    if high < low then Empty else Interval (low, high)
end;;

(* we can include that definition in a new module to create an extended version *)
module Extended_Interval = struct
    include Interval
    
    let contains t x = 
        match t with
        |Empty -> false
        |Interval (low,high) -> low && x <= high
end;;

