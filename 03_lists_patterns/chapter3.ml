

(* Chapter 3. Lists and Patterns 
 * 
 * An Ocaml list is an immutable, finite sequence of elements of the same type.
 * They can be generated using the bracket and semicolon notation:
 *)
let lis = [1;2;3];;

(* They can also be generated using the equivalent :: notation: 
 *)
let lis = 1 :: 2 :: 3 :: [];;
(* Note that :: is right-associative and the empty list [] is polymorphic 
 *
 * Lists in ocaml are singly linked:
        +---+---+   +---+---+   +---+---+
        | 1 | *---->| 2 | *---->| 3 | *---->||
        +---+---+   +---+---+   +---+---+ 
 * Each element has a link to the next. This is why an element can be
 * attached to the front of a list instantly.
 *)

(* Using Patterns:
 * We can read data with a match statement or the function keyword:
 *)
let rec sum ls = match ls with
    |[] -> 0
    |x::xs -> 1 + sum xs ;;
let rec sum' = function
    |[] -> 0
    |x::xs -> 1 + sum' xs ;;
(* '= function' is just syntactic sugar for 'x = match x with' *)

(* Performance:
 * Pattern matching is almost always more efficient than using if statements.
 * This is because the Ocaml compiler is usually able to generate machine code
 * that jumps straight to the matched case rather than checking them all.
 *
 * We can test this via benchmarking two functions using the core_bench library. *)
open Core.Std;;
open Core_bench.Std;;

let double_match = function
    |0 -> 0
    |1 -> 2
    |2 -> 4
    |3 -> 6
    |n -> n * 2  ;;
let double_if x = 
    if      x = 0 then 0 else if x = 1 then 2 
    else if x = 2 then 4 else if x = 3 then 6
    else x * 2 ;;

let run_bench tests = 
    Bench.bench 
        ~display_config: begin Bench.Display_config.create  
                ~ascii_table:true
                ~show_percentage:true 
                ~display:Textutils.Ascii_table.Display.column_titles 
                () end
        ~run_config: begin Bench.Run_config.create
            ~time_quota:(Core.Span.create ~sec:10 ())
            () end
        tests ;;
(*
[ Bench.Test.create ~name:"double_match" (fun () ->
    ignore (double_match 10))
; Bench.Test.create ~name:"double_if" (fun () ->
    ignore (double_if 10)) ]
|> run_bench 
;;
*)
(* Outputs the following:
 *   Estimated testing time 20s (change using -quota SECS).
 *                                           
 *     Name             Time/Run   Percentage  
 *    ---------------- ---------- ---------- 
 *     double_match      4.05ns       55.21%  
 *     double_if         7.34ns      100.00%  
 *
 * - Generally pattern matching is more efficient than the alternatives
 * - The error detecting capabilities of match statements are another benefit
 *)

(* Using the list module effectively - (map & fold) 
 * 
 *  Write a function render_table that, given a list of column headers and 
 *  a list of rows, prints them out in a well-formatted text table, as follows:
 *
 *      | language | architect      | first release |
 *      |----------+----------------+---------------|
 *      | Lisp     | John McCarthy  | 1958          |
 *      | C        | Dennis Ritchie | 1969          |
 *      | ML       | Robin Milner   | 1973          |
 ~ascii_table:true *      | OCaml    | Xavier Leroy   | 1996          |
 *)

(* Step one -> a function to compute the maximum width of each column *)

(* Take a function and a list and apply the function to each element:*)
List.map ~f:String.length ["Hello"; "World"];;
(*  - : int list = [5;6]                                                *)

(* List.map2_exn takes two lists and a function for combining them.
 * The '_exn' is there because this function throws an exception if the
 * lists are of unequal lengths *)
List.map2_exn ~f:Int.max [1;2;3] [3;2;1];;
(*  - : int list = [3;2;3]                                             *)

(* List fold is the most complicated of the three taking three args:
        - a list to process
        - an initial accumulator value
        - and a function for updating the accumulator at each element
eg: reverse a list                                                    *)
List.fold ~init:[] ~f:(fun ls x -> x :: ls) [1;2;3;4];; 

(* bring it all together:                                  *)
let max_widths header rows =
    let lengths l = List.map ~f:String.length l in
    List.fold rows
        ~init:(lengths header)
        ~f:(fun acc row -> 
            List.map2_exn ~f:Int.max acc (lengths row))
;;
let render_separator widths =
    let pieces = List.map widths
        ~f:(fun w -> String.make(w + 2) '-')
    in
    "|" ^ String.concat ~sep:"+" pieces ^ "|"
;;
let pad s length = " " ^ s ^ String.make(length - String.length s + 1) ' '
;;
let render_row row widths = 
    let padded = List.map2_exn  row widths ~f:(pad)
    in
    "|" ^ String.concat ~sep:"|" padded ^ "|" 
;;
let render_table header rows =
    let widths = max_widths header rows in
    String.concat ~sep:"\n"
    (render_row header widths
      :: render_separator widths
      :: List.map rows ~f:(fun row -> render_row row widths)
       @ [render_separator widths]
    )
;;
print_endline (
render_table ["Module"; "Credits"] [
            ["Elements of Functional Computing"; "10"]
            ;["Intro to Ai" ; "10"]
            ;["Software Workshop"; "40"]
            ;["Accounting and Finance";"20"]
            ]
);;

(* List.reduce is essentially a simpler version of List.fold that 
 * doesn't require a starting value as the accumulator consumes
 * and produces values of the same type as the list given  *)
List.reduce ~f:(+) [1;2;3;4;5];; (* -:int option = Some 15 *)

(* Filtering with List.filter and List.filter_map   *)
List.filter ~f:(fun x -> x mod 2 = 0) [1;2;3;4;5];;
(* -: int list = [2;4]
 * List.filter_map is a more powerful version of List.filter that
 * takes a function that returns an option and drops 
 * every None from it's resulting list *)
List.filter_map (Sys.ls_dir ".") ~f:(fun fname ->
        match String.rsplit2 ~on:'.' fname with
        |None 
        |Some("",_) -> None
        |Some(_,ext) -> Some ext)
        |> List.dedup
;;
(* -: string list = ["ascii"; "ml"; "native"; "txt"]  *)

(* Partitioning with List.partition_tf    (true/false)  *)
let is_ocaml_source s =
    match String.rsplit2 s ~on:'.' with
    |Some(_,("ml"|"mli")) -> true
    |_ -> false
;;
let (ml_files, other_files) =
    List.partition_tf (Sys.ls_dir ".") ~f:is_ocaml_source
;;

(* Combining Lists:          *)
[1;2;3] @ [4;5;6];;         (*
        =                    *)
List.append [1;2;3] [4;5;6];;

List.concat [[1;2];[3;4];[];[5];[6]];;

(* Example using List.concat and List.map to compute 
 * listing of a directory tree.         *)
let rec ls_rec s = 
    if Sys.is_file_exn ~follow_symlinks:true s
    then [s] else
    Sys.ls_dir s
    |> List.map ~f:(fun sub -> ls_rec (s ^/ sub))
    |> List.concat
;;
(* The above pattern is common enough that the
 * function List.concat_map combines the two functions *)
let rec ls_rec' s =
    if Sys.is_file_exn ~follow_symlinks:true s
    then [s] else
    Sys.ls_dir s
    |> List.concat_map ~f:(fun sub -> ls_rec (s ^/ sub))
;;

(*      Tail Recursion 
 * List.init -: int -> f:(int -> 'a) -> 'a list                *)
let make_list n = List.init n ~f:(fun x -> x);;
(*
 *   # length (make_list 10);;
 *   - : int = 10
 *   # length (make_list 10_000_000);;
 *   Stack overflow during evaluation (looping recursion?)
 * 
 *  A function call needs some space to keep track of 
 *  information associated with the call. To allow for
 *  nested function calls this information is stored in
 *  a 'stack'. So the 'stack overflow' above is caused by
 *  trying to allocate 10 million stack frames. 
 *
 *  The solution is Tail Recursion as tail calls don't need
 *  to be allocated a new stack frame thanks to tail-call optimization   *)
let make_list n =
    let rec aux acc = function
    |0 -> acc
    |n -> aux (n::acc) (n-1) in
    aux [] n
;;
make_list 10_000_000;;

(* Terser and Faster Patterns  *)
let destutter list =
    match list with 
    |[] -> []
    |[hd] -> [hd]
    |hd::hd'::tl ->
     if hd = hd' then destutter (hd'::tl)
     else hd :: destutter (hd' :: tl)
;; 
(* In the above we are recreating 
 * hd' :: tl after the else when
 * it already existed so instead 
 * we can use 'as' :    *)
let rec destutter' = function
    | [] -> []
    | [hd] -> [hd]
    | hd :: (hd' :: _ as tl) ->
        if hd = hd' then destutter' tl
        else hd :: destutter tl
;;
(* We can also further collapse this with an | pattern:   *)
let rec destutter'' = function
    | []|[_] as l -> l
    | hd :: (hd' :: _ as tl) ->
        if hd = hd' then destutter'' tl
        else hd :: destutter tl
;;
(* We can also make it terser by using a when clause:  *)
let rec destutter''2 = function
    | [] | [_] as l -> l
    | hd :: (hd' :: _ as tl) when hd = hd'-> destutter''2 tl
    | hd :: tl -> hd :: destutter''2 tl
;;
(* 'when' clause downside: the ability of the compiler to check
 *  exhaustiveness of patterns or if a case is redundant becomes
 *  compromised.                                                *) 

(* Polymorphic Compare     
 * Ocaml lets us test equality between values of any type  *)
3 = 4;;
[3;4;5] = [3;4;5];;
Some 3 = None;;
compare 6 4;; (*  :- int 1  *)
compare 3 3;; (*  :- int 0  *)
compare 4 6;; (*  :- int -1 *)
(* these polymorphic functions are built into the runtime
 * on a low level so you can't build functions like this 
 * yourself. These functions work by evaluating the structure
 * of the data in the memory. They will fail at runtime if they
 * are passed a function as a function cannot simply be compared 
 * to another function.                                 *)


(* end of chapter 3 *)

