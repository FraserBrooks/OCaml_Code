

open Printf;;

printf "-----------------------------------------------------------------------------------------------------\n
-----------------------------------------  99 PROBLEMS SOLVED IN OCAML  --------------------------------------\n";;


let p_ls ls =
    let rec aux = function
    |[] -> failwith "p_ls"
    |x::[] -> string_of_int x
    |x::xs -> string_of_int x ^ "; " ^ aux xs
    in
    "[" ^ aux ls ^ "] \n"
;;

let p_s_ls ?(nl = true) ls =
  let rec aux = function
    |[] -> failwith "p_s_ls"
    |x::[] -> x
    |x::xs -> x ^ "; " ^ aux xs
    in
    if nl then 
    "[" ^ aux ls ^ "] \n"
     else
    "[" ^ aux ls ^ "] " 
;;

let rec nest_ls_string ?(nl=true) ls =
    let rec aux = function
    |[] -> ""
    |x::y::tl -> p_s_ls x ~nl:false ^ "; " ^ aux (y::tl)
    |x::xs -> p_s_ls x ~nl:false ^ aux xs

    in
    if nl then
    "[" ^ aux ls ^ "] \n"
    else
    "[" ^ aux ls ^ "]"
;;



let test_list = [1;13;1;2;99;2;45;4;5;4;8];;
let one_to_ten = [1;2;3;4;5;6;7;8;9;10];;

printf "\ntest_list = %s" (p_ls test_list) ;;
printf "one_to_ten = %s" (p_ls one_to_ten) ;;







(* 1. last -: return the last element of a list *)
let rec last = function
    |[] -> failwith "last"
    |x::[] -> x
    |_::xs -> last xs
;;
print_endline "\n\n------------------------------------------ last -------------------------- 1:\n\tReturn the last element of a list: \n";;
printf "last one_to_ten = %d" (last one_to_ten);;


(* 2. last_but_one   *)
let rec last_but_one = function
    |[]         -> failwith "last_but_one"
    |x::y::[]   -> x
    |_::xs      -> last_but_one xs
;;
print_endline "\n\n------------------------------------------ last_but_one ----------------- 2:\n\tReturn the last but one element of a list: \n";;
printf "last_but_one one_to_ten = %d" (last_but_one one_to_ten);;

(* 3. Return Kth element  *)
let rec return_at k = function
    | [] -> failwith "return_at" (*Should use Option None*)
    | x::xs -> 
    if k = 0 then x else return_at (k-1) xs
;;
print_endline "\n\n------------------------------------------ return_at -----------------------3:\n\tReturn the Kth element of a list: \n";;
printf "return_at 4 one_to_ten = %d" (return_at 4 one_to_ten);;


(* 4. length of list *)
let len ls = 
    let rec aux k = function
    | [] -> k
    | _::xs -> aux (k+1) xs 
    in
    aux 0 ls
;;
print_endline "\n\n------------------------------------------- len -------------------------------4:\n\tReturn the length of a list: \n";;
printf "len one_to_ten = %d" (len one_to_ten);;


(* 5. Reverse a List *)
let rec rev = function
    | [] -> []
    | x::xs -> rev xs @ [x]
;;
print_endline "\n\n------------------------------------------- rev ----------------------------------5:\n\tReverse a list: \n";;
printf "rev one_to_ten = %s" (p_ls (rev one_to_ten));;


(* 6. Palindrome or not *)
let is_palindrome (ls: 'a list) =
    ls = rev ls
;;
print_endline "\n\n------------------------------------------- is_palindrome --------------------6:\n\tWhether or not a list is a palindrome: \n";;
printf "is_palindrome one_to_ten = %B" (is_palindrome one_to_ten);;
printf "\nis_palindrome ['a';'b';'b';'a'] = %B" (is_palindrome ['a';'b';'b';'a']);;

(* 7. Flatten a nested list structure  *)
type 'a node =
    | One of 'a
    | Many of 'a node list;;

let rec flatten = function
    | [] -> []
    | x::xs ->
        match x with
        | One a -> a :: flatten xs
        | Many ls -> flatten ls @ flatten xs
;;

let flatten' ls = 
    let rec aux acc = function
    |[] -> acc
    |One x::xs -> aux (x::acc) xs
    |Many y::ys -> aux (aux acc y) ys
    in
    List.rev (aux [] ls)
;; 

print_endline "\n\n------------------------------------------- flatten --------------------------7:\n\tFlatten a nested list: \n";;
let test_ls = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
print_endline "test_ls = [ One \"a\" ; Many [ One \"b\" ; Many [ One \"c\" ; One \"d\" ] ; One \"e\" ] ]i;;";;  
printf "flatten test_ls = %s" (p_s_ls (flatten test_ls));;
printf "flatten' test_ls = %s" (p_s_ls (flatten' test_ls));;



(* 8. Eliminate Consecutive duplicates of list elements *)
let compress ls =
    let rec aux last = function
    |[] -> []
    |x::xs -> if x = last then aux x xs else x :: aux x xs
    in
    match ls with
    |[] -> []
    |x::xs -> x :: aux x xs
;;

let rec compress' = function
    |x :: y :: ys -> if x = y then compress' (y::ys) else x :: (compress (y::ys))
    |smaller -> smaller
;;

print_endline "\n\n------------------------------------------- compress --------------------------8:\n\tRemove Consecutive Elements of a list: \n";;
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
print_endline "test_ls = [\"a\";\"a\";\"a\";\"a\";\"b\";\"c\";\"c\";\"a\";\"a\";\"d\";\"e\";\"e\";\"e\";\"e\"];;";; 
printf "compress test_ls = %s" (p_s_ls (compress test_ls));;
printf "compress' test_ls = %s" (p_s_ls (compress' test_ls));;

(*9  Pack consecutive elements of a list into sublists *)
let rec pack (ls: 'a list) : 'a list list =
    let rec aux (first : 'a) (ls : 'a list) (leftover : 'a list) : 'a list * 'a list = match leftover with
    |[] -> (ls, [])
    |x::xs -> if  first = x then (aux first (x::ls) xs ) else (ls, leftover)
    in
    match ls with
    |[] -> []
    |x::tl -> let (xs ,leftover_tl) = aux x [] tl in [x :: xs] @ pack leftover_tl
;;
 
print_endline "\n\n------------------------------------------- pack ------------------------------9:\n\tPack consecutive elements into sublists: \n";;
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
print_endline "test_ls = [\"a\";\"a\";\"a\";\"a\";\"b\";\"c\";\"c\";\"a\";\"a\";\"d\";\"e\";\"e\";\"e\";\"e\"];;";; 
printf "pack test_ls = %s" (nest_ls_string (pack test_ls));;

           
    
(* 10 Run-length encoding *)













print_endline "\n\n\n-------------------------------------------------------------------------------------
---------------------------------------------------------------------------------\n\n";;
