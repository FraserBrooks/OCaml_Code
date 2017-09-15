

open Printf;;

printf "-----------------------------------------------------------------------------------------------------\n
-----------------------------------------  99 PROBLEMS SOLVED IN OCAML  --------------------------------------\n
--------------------------------------(* Lists Section  (Problems 1-30) *)-------------------------------------";;


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

(*  CHECK CHECK CHECK *)


(* 20. Remove the K'th element from a list *)

let remove_at k ls =
    let rec aux acc n = function
    |[]    -> acc
    |x::xs -> match n with
        | 0 -> acc @ xs
        | _ -> aux (acc @ [x]) (n-1) xs
    in
    if k < 0 then failwith "remove_at" 
    else aux [] k ls
;;

let rec remove_at' k = function
    |[] -> []
    |x::xs -> if k = 0 then xs else x :: remove_at' (k-1) xs
;; (* Not tail recursive! *)


(* 21. Insert an element at a given position *)

let rec insert_at k el = function
    | [] -> [el]
    | x::xs as l -> if k = 0 then el :: l 
                else x :: (insert_at (k-1) el (xs))
;;(* Not tail recursive! *)

(* 22. Create a list containing all integers within a given range *)

let range ?(start=0) ~fin = 
    let rec aux count stop acc =
        if count = stop then acc @ [count]
        else aux (count + 1) stop (acc @ [count])
    in
    if start < fin then aux start fin [] 
    else List.rev(aux fin start [])
;;


(* 23. Extract a given number of randomly selected elements from a list *)

let rand_select ls k =
    let rec extract k acc = function
    |[] -> failwith "extract"
    |x::xs -> if k = 0 then (x, acc @ xs)
                else extract (k-1) (x :: acc) xs in
    let rec aux ls acc len = function
    | 0 -> acc
    | n -> let (found, rest) = extract (Random.int len) [] ls in
            aux rest (found::acc) (len-1) (n-1) in
    let len = List.length ls in
    aux ls [] len (min len k) 
;; (* Not very efficient *)

let rand_select' ls k =
    let bound = List.length ls in
    let rec aux acc (xs, ys) n = function
    |0 -> acc
    |i -> match ys with
        |[] -> aux acc (ys, xs) n i
        |h::tl -> if n = 0 then aux (h::acc) (xs,tl) (Random.int bound) (i-1)
                    else aux acc (h::xs, tl) (n-1) i
    in
    aux [] ([], ls) (Random.int bound) k
;; (* More efficient but still stuggles with very large lists (10million els) *)

let rand_select'' ls k =
    let bound = List.length ls in
    let delta = 1. /. ((float bound) *. 5. ) in
    let lotto_check hurdle bound =
        (float (Random.int bound) /. float bound) < hurdle in
    let rec aux acc (xs, ys) d = function
    |0 -> acc
    |i -> match ys with
        |[] -> aux acc (ys, xs) d i
        |h::tl -> if (lotto_check d bound) then aux (h::acc) (xs,tl) (delta) (i-1)
                    else aux acc (h::xs, tl) (d +. (delta *. delta)) i
    in
    aux [] ([], ls) (delta) k
;;  (* This is actually a lot less efficient than the above. Don't do this. *)

let rec randomise ls = 
    let split ls = 
        let rec aux (left, right) = function
        |[] -> (left, right)
        |x::x'::xs -> aux (x:: left, x'::right) xs
        |x::xs -> aux (x::left, right) xs
        in
        aux ([], []) ls 
    in
    let shuffle xs ys = 
        let rec aux acc = function
        |[],[] -> acc
        |(x::xs as ls), (y::ys as rs) ->
            if (Random.bool ()) then aux (x::acc) (xs,rs)
            else aux (y::acc) (ls,ys)
        |xs,[] -> (xs @ acc)
        |[],ys -> (ys @ acc) 
        in
        aux [] (xs,ys) 
    in
    let rec aux = function
        |[]|[_] as v -> v
        |ls -> 
            let (left, right) = split ls in
            let left' = aux left in
            let right' = aux right in
            shuffle left' right'
    in
    aux ls
;;

let rand_select''_ ls k =
    let rdm_ls = randomise ls in
    let rec aux acc k = function
    |[] -> acc
    |x::xs -> if k = 0 then acc 
              else aux (x::acc) (k-1) xs
    in
    aux [] k rdm_ls
;;
(* This seems to be the best solution so far as it's the only one capable
   of handling 10 million element lists in seemingly logarithmic time. *)

(* Merge-Sort -------------------

let split ls =
    let rec aux (left, right) = function
    |[] -> (left, right)
    |x::x'::xs -> aux (x:: left, x'::right) xs
    |x::xs -> aux (x::left, right) xs
    in
    aux ([], []) ls
;;
let merge xs ys =
    let rec aux acc = function
    |[],[] -> acc
    |(x::xs as ls),(y::ys as rs)-> 
            if x < y then aux (acc @ [x]) (xs,rs) 
            else aux (acc @ [y]) (ls, ys)
    |x::xs, [] -> aux (acc @ [x]) (xs, [])
    |[], y::ys -> aux (acc @ [y]) ([],ys)
    in
    aux [] (xs, ys) 
;;
let rec merge_sort = function
    |[]|[_] as v -> v
    |ls ->
        let (left, right) = split ls in
        let left' = merge_sort left in
        let right' = merge_sort right in
        merge left' right'
;;
(* Trying to create a tail recursive merge-sort like above is a 
    relatively pointless activity as the amount of stack space used by 
    merge-sort is O(lg n) so it's very unlikely you would ever exhaust the
    size of the available stack. Merge-sort itself is not purely tail recursive
    due to the divide and conquer approach. It's worth noting that it ~could~ be
    implemented in a tail recursive fashion using continuous passing style but this
    would be a relatively pointless endeavour and only work to make the code harder
    to understand (the only reason to do so would be to gain explicit control over
    the program control flow).
    The solution below is much more efficient.
*)
let rec merge' = function
    |[],[] -> []
    |xs,[] -> xs
    |[],ys -> ys
    |(x::xs as ls), (y::ys as rs) -> 
                    if x < y then x :: (merge' (xs,rs))
                    else y :: (merge' (ls,ys))
;;
let rec merge_sort' = function
    |[]|[_] as v -> v
    |ls -> 
        let (left,right) = split ls in
        let left' = merge_sort' left in
        let right' = merge_sort' right in
        merge' (left',right')
;;
 

let create_ls n k =
    let rec aux acc = function
    |0 -> acc
    |x -> aux ((Random.int n)::acc) (x-1) in
    aux [] k
;;

*)

(* 24. Lotto: Draw N different random numbers from the set 1..M *)

let lotto_draw range n =
    let rec create_list acc = function
    |0 -> acc 
    |x when x < 0 -> failwith "lotto_draw create_list"
    |x -> create_list (x::acc) (x-1)
    in
    let rnd_ls = randomise (create_list [] range)
    in 
    let rec aux acc n = function
    |[] -> acc
    |x::xs -> if n = 0 then acc 
                else aux (x::acc) (n-1) xs
    in
    aux [] n rnd_ls
;;
(* Solution given on website builds upon previous two solutions instead *)

(* 25. Randomise a list *)

let rec randomise ls = 
    let split ls = 
        let rec aux (left, right) = function
        |[] -> (left, right)
        |x::x'::xs -> aux (x:: left, x'::right) xs
        |x::xs -> aux (x::left, right) xs
        in
        aux ([], []) ls 
    in
    let shuffle xs ys = 
        let rec aux acc = function
        |[],[] -> acc
        |(x::xs as ls), (y::ys as rs) ->
            if (Random.bool ()) then aux (x::acc) (xs,rs)
            else aux (y::acc) (ls,ys)
        |xs,[] -> (xs @ acc)
        |[],ys -> (ys @ acc) 
        in
        aux [] (xs,ys) 
    in
    let rec aux = function
        |[]|[_] as v -> v
        |ls -> 
            let (left, right) = split ls in
            let left' = aux left in
            let right' = aux right in
            shuffle left' right'
    in
    aux ls
;;

(* My solution from before. Their solution (shown below) can't handle large lists.*)
let rec permutation list =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux acc list len =
      if len = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (picked :: acc) rest (len-1)
    in
    aux [] list (List.length list);;


(* 26. Generate the combinations of K distinct objects chosen from 
        the N elements of a list.
    (There are C(N,K) possibilities with C(N,K) denoting the binomial coefficients)
    ie. extract 2 ["a";"b";"c"];; -: [["a";"b"];["a";"c"];["b";"c"]]   *)
let rec binomial_extract ls = function
    | 0 -> [ [] ]
    | k -> match ls with
        | [] -> []
        | x::xs -> 
            let with_head = List.map (fun ls -> x :: ls) 
                                (binomial_extract xs (k-1)) in
            let without = binomial_extract xs k in
            with_head @ without
;;
(* spotty dog! *)

(* 27. (.28) In how many ways can 9 people work in disjoint subgroups of 2,3 and 4?  *)

let group ls sizes =
    let extract k ls =
        if ls = [] then failwith "group - not enough elements" else
        let rec aux ls = function
        | 0 -> [ ([], ls) ]
        | k -> match ls with
            | [] -> []
            | x::xs -> 
                let with_head = List.map (fun (ls,leftover) -> (x :: ls, leftover)) 
                                    (aux xs (k-1)) in
                let without = List.map (fun (ls,leftover) -> (ls, x::leftover)) 
                                    (aux xs k) in
                with_head @ without
        in
        aux ls k
    in
    let rec aux ls group_sizes = match group_sizes with
    | [] -> [[[]]]
    | x::xs -> let processed = extract x ls in
        List.fold_left 
            (fun acc (*list list list (depth 1)*) (ys,leftover) -> 
                acc @ (        
                    List.map (fun nested_list (*depth 2*) ->
                        match nested_list with
                        |[] -> [ys]
                        |x::_  -> if x = [] then [ys] else [ys] @ nested_list
                    ) (aux leftover xs)
                )
            ) [] processed
    in
    aux ls sizes
;;
(* ocaml.org solution:  *)

(* This implementation is less streamlined than the one-extraction
    version, because more work is done on the lists after each
    transform to prepend the actual items. The end result is cleaner
    in terms of code, though. *)
  
let group' list sizes =
    let initial = List.map (fun size -> size, []) sizes in
  
    (* The core of the function. Prepend accepts a list of groups,
       each with the number of items that should be added, and
       prepends the item to every group that can support it, thus
       turning [1,a ; 2,b ; 0,c] into [ [0,x::a ; 2,b ; 0,c ];
       [1,a ; 1,x::b ; 0,c]; [ 1,a ; 2,b ; 0,c ]]
  
       Again, in the prolog language (for which these questions are
       originally intended), this function is a whole lot simpler.  *)
    let prepend p list =
      let emit l acc = l :: acc in
      let rec aux emit acc = function
        | [] -> emit [] acc
        | (n,l) as h :: t ->
           let acc = if n > 0 then emit ((n-1, p::l) :: t) acc
                     else acc in
           aux (fun l acc -> emit (h :: l) acc) acc t
      in
      aux emit [] list
    in
    let rec aux = function
      | [] -> [ initial ]
      | h :: t -> List.concat (List.map (prepend h) (aux t))
    in
    let all = aux list in
    (* Don't forget to eliminate all group sets that have non-full
       groups *)
    let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
    List.map (List.map snd) complete;;



(* 29. Sort a list of lists according to sublist length *)

let length_sort ls =
    let ls_with_lengths = 
        List.fold_left (fun acc ls -> (List.length ls, ls)::acc) [] ls 
    in     
    let split ls =
        let rec aux (left, right) = function
        |[] -> (left, right)
        |x::x'::xs -> aux (x:: left, x'::right) xs
        |x::xs -> aux (x::left, right) xs
        in
        aux ([], []) ls
    in
    let merge xs ys =
        let rec aux acc = function
        |[],[] -> acc
        |(x::xs as ls),(y::ys as rs)-> 
                if x > y then aux (acc @ [x]) (xs,rs) 
                else aux (acc @ [y]) (ls, ys)
        |x::xs, [] -> aux (acc @ [x]) (xs, [])
        |[], y::ys -> aux (acc @ [y]) ([],ys)
        in
        aux [] (xs, ys) 
    in
    let rec merge_sort = function
        |[]|[_] as v -> v
        |ls ->
            let (left, right) = split ls in
            let left' = merge_sort left in
            let right' = merge_sort right in
            merge left' right'
    in
    List.fold_left (fun acc (_,ls) -> ls::acc) 
        [] (merge_sort ls_with_lengths) (* Making use of polymorphic compare *)
;; 

(* ocaml.org solution: - see solution to #30 *)

(* 30. Sort a list of lists according to sublist length FREQUENCY *)

let frequency_sort ls =
    let ls_with_lengths = 
        List.fold_left (fun acc ls -> (List.length ls, ls)::acc) [] ls 
    in
    let rec count_freq count last = function
    |[] -> count
    |(x,_)::tl -> if x = last then count_freq (count + 1) x tl
                    else count
    in
    let rec with_freq acc visited freq = function
    |[] -> acc
    |(x,ls)::tl -> if visited > 0 then with_freq ((freq,ls)::acc) (visited-1) freq tl
                    else let count = count_freq 1 x tl 
                    in
                    with_freq ((count,ls)::acc) (count-1) count tl
    in    
    let split ls =
        let rec aux (left, right) = function
        |[] -> (left, right)
        |x::x'::xs -> aux (x:: left, x'::right) xs
        |x::xs -> aux (x::left, right) xs
        in
        aux ([], []) ls
    in
    let merge xs ys =
        let rec aux acc = function
        |[],[] -> acc
        |(x::xs as ls),(y::ys as rs)-> 
                if x < y then aux (acc @ [x]) (xs,rs) 
                else aux (acc @ [y]) (ls, ys)
        |x::xs, [] -> aux (acc @ [x]) (xs, [])
        |[], y::ys -> aux (acc @ [y]) ([],ys)
        in
        aux [] (xs, ys) 
    in
    let rec merge_sort = function
        |[]|[_] as v -> v
        |ls ->
            let (left, right) = split ls in
            let left' = merge_sort left in
            let right' = merge_sort right in
            merge left' right'
    in
    let sorted_by_lengths = (merge_sort ls_with_lengths) (* Making use of polymorphic compare *)
    in
    let with_frequency = with_freq [] 0 0 sorted_by_lengths
    in
    List.map snd (merge_sort with_frequency)
;; (* ^What a mess! *)

(* ocaml.org solution:  *)
(* We might not be allowed to use built-in List.sort, so here's an
     eight-line implementation of insertion sort  O(n) time
     complexity. *)
  let rec insert cmp e = function
    | [] -> [e]
    | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t
  
  let rec sort cmp = function
    | [] -> []
    | h :: t -> insert cmp h (sort cmp t)
  
  (* Sorting according to length : prepend length, sort, remove length *)
  let length_sort lists =
    let lists = List.map (fun list -> List.length list, list) lists in
    let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
    List.map snd lists;;

(* Sorting according to length frequency : prepend frequency, sort,
     remove frequency. Frequencies are extracted by sorting lengths
     and applying RLE to count occurences of each length (see problem
     "Run-length encoding of a list.") *)
  let rle list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (x, count + 1) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (count + 1) acc t
         else aux 0 ((a, count + 1) :: acc) t in
    aux 0 [] list
  
  let frequency_sort' lists =
    let lengths = List.map List.length lists in
    let freq = rle (sort compare lengths) in
    let by_freq =
      List.map (fun list -> List.assoc (List.length list) freq , list) lists in
    let sorted = sort (fun a b -> compare (fst a) (fst b)) by_freq in
    List.map snd sorted;; 



(* End of 'Working with Lists' section  *)


print_endline "\n\n\n-------------------------------------------------------------------------------------
---------------------------------------------------------------------------------\n\n";;
