

open Printf;;

printf "-----------------------------------------------------------------------------------------------------\n
-----------------------------------------  99 PROBLEMS SOLVED IN OCAML  --------------------------------------\n";;


let string_of_int_ls ls =
    let rec aux = function
    |[] -> ""
    |x::[] -> string_of_int x
    |x::xs -> string_of_int x ^ "; " ^ aux xs
    in
    "[" ^ aux ls ^ "] \n"
;;
let string_of_string_ls ?(nl = true) ls =
  let rec aux = function
    |[] -> ""
    |x::[] -> x
    |x::xs -> x ^ "; " ^ aux xs
    in
    if nl then 
    "[" ^ aux ls ^ "] \n"
     else
    "[" ^ aux ls ^ "] " 
;;
let string_of_string_ls_ls ?(nl=true) ls =
    let rec aux = function
    |[] -> ""
    |x::y::tl -> string_of_string_ls x ~nl:false ^ "; " ^ aux (y::tl)
    |x::xs -> string_of_string_ls x ~nl:false ^ aux xs

    in
    if nl then
    "[" ^ aux ls ^ "] \n"
    else
    "[" ^ aux ls ^ "]"
;;

let test_foo foo name num desc
                input input_name string_of_input
                        string_of_result =
    let rec buff = function
    | 0 -> " "
    | x -> if x < 0 then failwith "test_arr buff" 
            else "-" ^ buff (x-1)
    in
    let len = String.length name
    in
    let _ = print_endline (("\n\n") ^ (buff 35) ^ (name) ^ (buff (35-len)) ^ (num) ^ ("\n\t") ^ (desc) ^ ("\n\n"));
    in
    let _ = print_endline ( "\ninput: " ^ input_name ^ " = " ^ ( input |> string_of_input ) );
    in
    let prefix = name ^ " " ^ input_name
    in
    printf "%s = %s" prefix  ( input |> foo |> string_of_result )
;;

let test_list = [1;13;1;2;99;2;45;4;5;4;8];;
let one_to_ten = [1;2;3;4;5;6;7;8;9;10];;

test_list |> string_of_int_ls |> printf "\n test_list = %s"  ;;
one_to_ten |> string_of_int_ls |> printf "\n one_to_ten = %s";;





(* 1. last -: return the last element of a list *)
let rec last = function
    |[] -> failwith "last"
    |x::[] -> x
    |_::xs -> last xs
;;
test_foo last "last" "1." "Return the last element of a list" one_to_ten "one_to_ten" string_of_int_ls (fun x -> string_of_int x);;

(* 2. last_but_one   *)
let rec last_but_one = function
    |[]         -> failwith "last_but_one"
    |x::_::[]   -> x
    |_::xs      -> last_but_one xs
;;
test_foo last_but_one "last_but_one" "2." "Return the last but one element of a list" one_to_ten "one_to_ten" string_of_int_ls (fun x -> string_of_int x);;

(* 3. Return Kth element  *)
let rec return_at k = function
    | [] -> failwith "return_at" (*Should use Option None*)
    | x::xs -> 
    if k = 0 then x else return_at (k-1) xs
;;
test_foo (4 |> return_at) "return_at 4" "3." "Return the kth element of a list" one_to_ten "one_to_ten" string_of_int_ls (fun x -> string_of_int x);;

(* 4. length of list *)
let len ls = 
    let rec aux k = function
    | [] -> k
    | _::xs -> aux (k+1) xs 
    in
    aux 0 ls
;;
test_foo len "len" "4." "Return the length of a list" one_to_ten "one_to_ten" string_of_int_ls (fun x -> string_of_int x);;

(* 5. Reverse a List *)
let rec rev = function
    | [] -> []
    | x::xs -> rev xs @ [x]
;;
test_foo rev "rev" "5." "Reverse a list" one_to_ten "one_to_ten" string_of_int_ls string_of_int_ls;;

(* 6. Palindrome or not *)
let is_palindrome (ls: 'a list) =
    ls = rev ls
;;
test_foo is_palindrome "is_palindrome" "6." "Whether or not list is palindrome" one_to_ten "one_to_ten" string_of_int_ls (fun x -> string_of_bool x);;
printf "\nis_palindrome ['a';'b';'b';'a'] = %B" (is_palindrome ['a';'b';'b';'a']);;



(* 7. Flatten a nested list structure  *)
type 'a node =
    | One of 'a
    | Many of 'a node list
;;
let string_of_nest_string_ls ?(nl=true) ls =
    let rec aux = function
    |[] -> ""
    |x::y::tl -> (match x with 
        | One x   -> "One " ^ x ^ ";"  ^ aux (y::tl)
        | Many xs -> "Many [" ^ aux xs ^ "];"  ^ aux (y::tl)
        )
    |x::tl -> ( match x with 
        | One x   -> "One " ^ x ^ aux tl
        | Many xs -> "Many [" ^ aux xs ^ "]"  ^ aux tl
        )
    in
    if nl then
    "[" ^ aux ls ^ "] \n"
    else
    "[" ^ aux ls ^ "]"
;;
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
let test_ls = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
test_foo flatten "flatten" "7." "Flatten a nested list" test_ls "test_ls" string_of_nest_string_ls string_of_string_ls;;
printf "flatten' test_ls = %s" (test_ls |> flatten' |> string_of_string_ls);;




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
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
test_foo compress "compress" "8." "Eliminate consecutive duplicates" test_ls "test_ls" string_of_string_ls string_of_string_ls;;
printf "compress' test_ls = %s" (test_ls |> compress' |> string_of_string_ls);;




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
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
test_foo pack "pack" "9." "Pack consecutive elements into sublists" test_ls "test_ls" string_of_string_ls string_of_string_ls_ls;;

   
(* 10 Run-length encoding *)
let string_from_rle ls =
    let rec aux = function
    | [] -> ""
    | (x, y)::a::xs -> "(" ^ (string_of_int x) ^ ", " ^ y ^ ") ;" ^ aux (a::xs)
    | (x, y)::_ -> "(" ^ (string_of_int x) ^ ", " ^ y ^ ")"
    in
    " [ " ^ (aux ls) ^ " ] \n"
;;
let encode ls = 
    let rec aux count = function
    | [] -> []
    | x::y::tl -> if x = y then aux (count + 1) (y::tl)
                    else (count, x) :: aux 1 (y::tl)
    | x::_ -> [(count, x)]
    in
    aux 1 ls
;;
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
test_foo encode "encode" "10." "Run-Length encoding of a list" test_ls "test_ls" string_of_string_ls string_from_rle;;

(* 11 Modified Run-Length Encoding *)
type 'a rle =
    | One of 'a
    | Many of int * 'a
;;
let string_from_rle' ls =
    let rec aux = function
    | [] -> ""
    | hd::a::xs -> begin match hd with
                | One k -> "One '" ^ k ^ "'; " ^ aux (a::xs)
                | Many (b,c) -> "Many (" ^ (string_of_int b) ^ ", '" ^ c ^ "'); " ^ aux (a::xs)
                end
    | hd::_ ->  begin match hd with
                | One k -> "One '" ^ k ^ "' "
                | Many (b,c) -> "Many (" ^ (string_of_int b) ^ ", '" ^ c ^ "') "
                end
    in
    " [ " ^ (aux ls) ^ " ] \n"
;;
let encode' ls = 
    let new_entry a = function
    | 1 -> One a
    | x -> Many (x, a)
    in
    let rec aux count = function
    | [] -> []
    | x::y::tl -> if x = y then aux (count + 1) (y::tl)
                    else (new_entry x count) :: aux 1 (y::tl)
    | x::_ -> [new_entry x count]
    in
    aux 1 ls
;;
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
test_foo encode' "encode'" "11." "Modified run-length encoding of a list" test_ls "test_ls" string_of_string_ls string_from_rle';;

(* 12. Decode a run-length encoded list *)
let rec decode ls =
    let rec aux el = function
    |0 -> []
    |x -> el :: aux el (x-1)
    in
    let get_next_el = function
    |One a -> [a]
    |Many (b,c) -> aux c b
    in
    match ls with
    |[] -> []
    |x::xs -> (get_next_el x) @ decode xs
    ;;
let test_ls = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
test_foo decode "decode" "12." "Decode a run-length encoded list" test_ls "test_ls" string_from_rle' string_of_string_ls;;

(* 13. Run-length encoding of a list (direct solution) *)
let encode_' ls = 
    let rec next el count = if count = 0 then One el else Many (count, el) in
    let rec aux count acc = function
    | [] -> [] (* Only reached with empty original list *)
    | [x] -> next x count :: acc
    | a :: (b::_ as t) -> if a = b then aux (count+1) acc t
                            else aux 1 (next a count :: acc) t in
    List.rev (aux 1 [] ls)
;;
let test_ls = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
test_foo encode_' "encode_'" "13." "Direct run-length encoding of a list" test_ls "test_ls" string_of_string_ls string_from_rle';;

(* 14. Duplicate the elements of a list *)
let duplicate ls =
    let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (x::x::acc) xs in
    List.rev (aux [] ls)
;;
let a_to_d = ["a";"b";"c";"c";"d"];;
test_foo duplicate "duplicate" "14." "Duplicate every element in a list" a_to_d "a_to_d" string_of_string_ls string_of_string_ls;;

(* 15. Replicate the elements of a list k times *)
let replicate k ls =
    let rec copy el acc = function
    | 0 -> acc
    | x -> copy el (el::acc) (x-1) in
    let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (copy x acc k) xs in
    List.rev (aux [] ls)
;;
let a_to_d = ["a";"b";"c";"c";"d"];;
test_foo (replicate 4) "replicate 4" "15." "Replicate the elements of a list K times" a_to_d "a_to_d" string_of_string_ls string_of_string_ls;;

(* 16. Drop every Nth element from a list *)
let drop k ls = 
    let rec aux acc i = function
    |[] -> acc
    |x::xs -> if i = 0 then aux acc (k-1) xs else aux (x::acc) (i-1) xs in
    List.rev (aux [] (k-1) ls)
;;
let a_to_j = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"];;
test_foo (drop 3) "drop 3" "16." "Drop every Nth element of a list" a_to_j "a_to_j" string_of_string_ls string_of_string_ls;;

(* 17. Split a list into two parts at K *)
let split k ls = 
    let rec aux k (a,b) = function
    |[] -> (List.rev(a),b)
    |x::xs -> if k = 0 then (List.rev(a),xs) else aux (k-1) (x::a, []) xs in
    aux k ([],[]) ls
;;
let test_ls = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"];;
test_foo (split 3) "split 3" "17." "Split a list given the length of the first part" test_ls "test_ls" string_of_string_ls (fun (x,y) -> (string_of_string_ls x) ^ (string_of_string_ls y));;
test_foo (split 200) "split 200" "" "" test_ls "test_ls" string_of_string_ls (fun (x,y) -> (string_of_string_ls x) ^ (string_of_string_ls y));;

(* 18. Extract a slice from a list *)
let slice i k ls =
    let rec aux i k acc = function
    |[] -> List.rev acc
    |hd::tl -> if i = 0 
                then 
                    begin 
                    if k = 0 then List.rev (hd::acc) else aux i (k-1) (hd::acc) tl
                    end
                else
                    aux (i-1) (k-1) acc tl
    in
    aux i k [] ls  
;;
let a_to_j = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"];;
test_foo (slice 2 6) "slice 2 6" "18." "Extract a slice from a list" a_to_j "a_to_j" string_of_string_ls string_of_string_ls;;
test_foo (slice 2 56) "slice 2 56" "" "" a_to_j "a_to_j" string_of_string_ls string_of_string_ls;;

(* 19. Rotate a list N places to the left *)
let rotate k ls =
    let rec aux ls = function
    | 0 ->ls
    | x ->match ls with
            |[] -> []
            |hd::tl -> aux (tl @ [hd] ) (x-1)
    in
    if k < 0 then List.rev(aux (List.rev ls) (k * (-1)))
    else aux ls k
;;
let a_to_h = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];;
test_foo (rotate 3) "rotate 3" "19." "Rotate a list N places to the left" a_to_h "a_to_h" string_of_string_ls string_of_string_ls;;
test_foo (rotate (-2)) "rotate (-2)" "" "" a_to_h "a_to_h" string_of_string_ls string_of_string_ls;;




































print_endline "\n\n\n-------------------------------------------------------------------------------------
---------------------------------------------------------------------------------\n\n";;
