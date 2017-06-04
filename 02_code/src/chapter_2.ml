
open Core.Std;;

(* Chapter 2 - Real World OCaml *)

(* Variables and Functions *)

let languages = "Ocaml,Perl,C++,C";;
let dashed_languages =
    let language_list = String.split languages ~on:',' in
    String.concat ~sep:"-" language_list
;;

(* let bindings support pattern matching:             *)
let (ints,strings) = List.unzip [(1,"one"); (2, "two"); (3, "three")];;
(* using pattern matching in this way makes the most sense for patterns that
   are ~irrefutable~ ie. where any value of the type in question is guaranteed 
   to match the pattern.
   For example tuples and records are irrefutable but lists are not (could be [_] or []
*)
let upcase_first_entry line = 
    let (first::rest) = String.split ~on:',' line (*returns a list that is matched to first and rest *)
    in
    String.concat ~sep:"," (String.uppercase first :: rest)
;;
(* The compiler will give a warning about the above because String.split could
   return an empty list for all the compiler knows. In practice it doesn't but it
   is still generally better to use a match statement like below:
*)
let upcase_first_entry' line =
    match String.split ~on:',' line with
    | [] -> assert false (* String.split returns at least one element *)
    | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
;;

(* FUNCTIONS *)

(* Anonymous Functions *)
(* declared without being named via the 'fun' keyword *)
(fun x -> x *. x);;

(fun x -> x *. x) 5.;;

List.map ~f: (fun x -> x *. x) [2.;3.;4.;5.;6.];;

(*can even put them in a data structure *)
let increments = [ (fun x -> x + 1) ; (fun x -> x + 2) ; (fun x -> x + 3)];;
List.map ~f:(fun i -> i 5) increments;; (* the ~fun~ here is taking a function as an argument and applying it to the number 5 *)

(* the key thing to understand is that functions in ocaml are just like any other data type *)

let plusone = (fun x -> x + 1);;
(* Syntactic Sugar for: *)
let plusone' x = x + 1;;

(* function parameters are similar to let bindings *)
(* the following are identical in a way: *)
(fun x -> x + 1) 7;;
let x = 7 in x + 1;;

(* Multiargument functions *)
let abs_diff x y = abs (x - y);;
(* becuase of the syntactic sugar it is not clear exactly what is going on here *)
(* if we re-write it verbosley:   *)
let abs_diff' = 
    (fun x -> ( fun y -> abs (x - y)));;
(*  This makes it clear that abs_diff is actually just a one argument function that
    returns another one argument function that gives us the result we want.
    
    This style of function is called a 'curried' function (Named after Haskell Curry)

    You can make use of currying to specialise a function by feeding in some of the arguments *)
let dist_from_3 = abs_diff 3;;
dist_from_3 4;;
dist_from_3 (-47);;
(* This is called partial application - ( getting a new function via applying some of the arguments ) *)

(* Currying is not the only way of writing multiargument functions. Can also use tuples: *)
let abs_diff'' (x,y) = abs (x - y);;

(* Recursive Functions  *)
let rec find_first_stutter list =
    match list with
    | [] | [_] ->
      (* only zero or one elements, so no repeats *)
      None
    | x :: y :: tl ->
      if x = y then Some x else find_first_stutter (y::tl)
   ;;

(*  We can also define multiple mutually recursive values by using let rec 
    combined with the and keyword. Here's a (gratuitously inefficient) example:  *)
let rec is_even x =
    if x = 0 then true else is_odd (x - 1)
  and is_odd x =
    if x = 0 then false else is_even (x - 1)
 ;;

(* Prefix and Infix Operators  *)
Int.max 3  4 (* prefix *);;
3 + 4        (* infix *);;
(*  operators are only really syntactically different from functions
    we can use operators as prefix functions with parentheses like so:  *)
(+) 3 4;;

List.map ~f:((+) 3) [4;5;6];;

(* A function is treated syntactically as an operator if the name is made up 
   only of the following characters:
    
    ! $ % & * + - . / : < = > ? @ ^ | ~

    This means we can define our own operators:
*)

let ( *! ) x y = ((x) *. (x *. x)) +. y;; (* Cube x then + y *)
3. *! 1.;;
4. *! 1.;;
32. *! 1.;;

let ( ^! ) x = x *. x *. x;;
(^!) 5.;;

(* Useful operator from the standard library   |>  *)
(*  let (|>) x f = f x;; 
    val ( |> ) : a' -> ('a -> b') -> b' = <fun>
    
    |> takes a value and a function and applies the
    value to the function which sounds useless but 
    it's usefullness comes from the fact that it is
    left-associative:
*)

let names = "Fraser:Rebecca:John:Fraser:Max:Tom:Tom:Rebecca:Georgia:Jill";;
let de_duplicate_and_print = 
    String.split ~on:':' names
    |> List.dedup ~compare:String.compare
    |> List.iter ~f:print_endline
    ;;

(* The line: x |> y ;;  can be thought of as apply function 'y' to value 'x' 
    Notice how much more verbose the code is without |>   :
*)

let split_names = String.split ~on:':' names in
  let deduped_names = List.dedup ~compare:String.compare split_names in
  List.iter ~f:print_endline deduped_names
  ;;

(* Declaring Functions with function  *)

let some_or_0 = function
    | Some x -> x
    | None -> 0
;;
List.map ~f:some_or_0 [Some 3; Some 4; None; Some 41];;
(*this is equivalent to... *)

let some_or_0' num = 
    match num with
    | Some x -> x
    | None -> 0
;;
List.map ~f:some_or_0' [Some 3; Some 4; None; Some 41];;

(* Labeled Arguments   ------------- *)

let ratio ~num ~denom = float num /. float denom;;

ratio ~num:3 ~denom:100;;
ratio ~denom:100 ~num:3;;

(*label punning -  if the name of the label matches the name of the variable
                   we can drop the text after the :
*)
let num = 3 in
let denom = 10 in
ratio ~num ~denom;;

(* Labeled arguments are usefull:

    - when defining a function with many arguments. 
      (After a certain point it becomes easier to remember names over position)

    - when the meaning of a particular argument is unclear from the type alone
      (for example:
            val create_hashtable :
                init_size:int -> allow_shrinking:bool -> ('a,'b) Hashtable.t
        is clearer than:
            val create_hashtable : int -> bool -> ('a,'b) Hashtable.t

    - when defining a function with arguments of a similar but distinct nature
        (for example start_int and end_int  )

    - when you want flexibilit in the order arguments are passed to allow for
      partial application. For example with List.iter

High Order Functions and Labels:
        Need to keep same order of labeled arguments when passing labeled
        functions as arguments.
*)

(*  -----------    Optional Arguments *)

let concat ?sep x y =
    let sep = match sep with None -> "" | Some x -> x in
    x ^ sep ^ y
;;
concat "Ignore the " "Optional Argument";;
concat "Use" ~sep:" the " "Optional Argument";;

(* There is a neater way to write the above function. Eliminating the
    need to use a match to set a default value.
    eg.                                                   *)
let concat' ?(sep="") x y = x ^ sep ^ y;;
concat "a" "c";;
concat "a" ~sep:"b" "c";;

(* Explicit passing of an optional argument  *)

let uppercase_concat ?(sep="") a b = concat ~sep (String.uppercase a) b ;;
(* In the function above we have explicitly given concat the optional sep parameter
    as either "" (the default) or as whatever the caller supplied.
    If we ever want to change the default behaviour of concat we would now have to remember to 
    change the default behaviour of this function too. To avoid this we can explicitly pass the optional
    argument straight through like so:                                                                  *)
let uppercase_concat' ?sep a b = concat ?sep (String.uppercase a) b ;;


(* ---- Inference of labeled and optional arguments    *)

let numeric_deriv ~delta ~x ~y ~f = (* *)
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~x:x' ~y -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx, dy)
;;

let poly ~x ~y = x *. x +. y *. y;;


numeric_deriv ~delta:2. ~x:2. ~y:2. ~f:poly;; (* I have no idea what this is doing *)


(* Optional arguments and partial application 
    The compiler will assume an optional argument is not being given
    when the positional argument after it is given. At this point 
    (in the case of partial application) the optional argument will 
    no longer be available.                                             *)


let prepend_hash = concat "# ";;
(* As optional argument was first in our definition it has now dissapeared
    and can't be given with a call to prepend_hash.
    If we move the optional argument down:                          *)
let concat x ?(sep="") y = x ^ sep ^ y ;;
(* Now we can *)

let prepend_hash = concat "# ";;

prepend_hash "a BASH comment" ~sep:"--- ";;

(* If the optional argument is placed at the end of the argument list
    then the compiler will issue a warning as it will never be able to 
    know if it is not being specified. This would kind of defeat the point
    of an optional argument as you'd always need to supply it.              *)





