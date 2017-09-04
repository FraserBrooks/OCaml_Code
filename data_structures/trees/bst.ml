

Random.self_init();;
let brk = "----------------------------------------------------------"
          ^"---------------------------------------------------------";;

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;
type tree_comp = Eq|Lt|Rt

(*
let n13 = Node(13, Empty, Empty)
let n12 = Node(12, Empty, Empty)
let n11 = Node(11, Empty, Empty)
let n10 = Node(10, Empty, Empty)
let n9 = Node(9, Empty, Empty)
let n8 = Node(8, Empty, Empty)

let n7 = Node(7, Empty,Empty)
let n6 = Node(6, n12, n13)
let n5 = Node(5, n10, n11)
let n4 = Node(4, n8, n9)
let n3 = Node(3, n6, n7)
let n2 = Node(2, n4,n5)
let test_tree = Node(1, n2, n3)
*)

let left = function 
    Node (_, l, _) -> l
    |Empty -> failwith "left"
;;

let right = function
    |Node (_,_,r) -> r
    |Empty -> failwith "right"
;;

let key_of = function
    |Node (v,_,_) -> v
    |Empty -> failwith "key_of"
;;

let rec buffer = function
    |0 -> ""
    |n -> " " ^ buffer (n-1)
;;

let rec count = function
    |Empty -> 0
    |Node (_, left, right) ->
        1 + (count left) + (count right)
;;


let rec depth = function
    | Empty -> 0
    | Node (_, left, right) ->
        1 + max (depth left) (depth right)
;;

let rec nodes_at k = function
    |Empty -> if k = 0 then [None] else [None] @ (nodes_at (k-1) Empty)
    |Node(_,l,r) as x ->
        if k = 0 then [Some x] 
        else (nodes_at (k-1) l) 
                @ (nodes_at (k-1) r)
;;

let depth_order t = 
    let rec aux root max_depth x =
        if x = max_depth
            then []
            else (x, nodes_at x root) ::
                 aux root max_depth (x+1)
    in
    aux t (depth t) 0
;;
 
(* Converting a bst to a printable string. Works well enough on small trees *)
let string_of_tree t string_foo order =
    let rec get_line k  = function
    |[] -> ""
    |x::xs -> match x with
        |None ->        
            buffer k
            ^ "(#)" 
            ^ buffer k 
            ^ get_line k xs
        |Some x -> match x with Empty -> failwith "get_line"|(Node (x,_,_)) -> 
            buffer k
            ^ "(" ^ string_foo x ^ ")" 
            ^ buffer k 
            ^ get_line k xs
    in
    let rec aux = function
    |[] -> "\n"
    |(n, xs)::ys -> let nf = float_of_int n in (* Current Level *)
                    let o = float_of_int order in (* Order of tree (2 for bst, 4 for quad)*)
                    let a = int_of_float (o ** nf) in  (* Number of nodes on current level *)
                    let b = (a * 2) + 2 in (* number of buffers *)
                    let k = (120/b) -  ((2 * a)/b) in
                    (buffer k)
                    ^ (get_line  k xs)
                    ^ (buffer k)
                    ^ "\n"
                    ^ (aux ys)
    in
    aux (depth_order t)
;;
let p_i_tree tree = print_endline((string_of_tree tree string_of_int 2)^brk^"\n");;

let rotright = function
    |Empty -> Empty
    |Node (v, l, r) ->
        match l with
        |Empty -> failwith "Cannot rotate right"
        |Node (v', l', r') -> Node(v', l', Node(v, r', r))
;;
let rotleft = function
    |Empty -> Empty
    |Node (v, l, r) ->
        match r with
        |Empty -> failwith "Cannot rotate right"
        |Node (v', l', r') -> Node (v', Node(v, l, l'), r')
;;


let rec compress tree = function
    |0 -> tree
    |x -> match tree with
        |Empty -> Empty
        |Node(_,_,r) -> match r with
            |Empty -> tree
            |Node(_,_,_) -> 
                let root = rotleft tree in
                let v = key_of root in
                let l = root |> left in
                let r = root |> right in
                Node(v, l, compress  r (x-1))
;;
let compress' tree num = 
    let rec loop tree num k = match num with
    |0 -> k tree
    |x -> match tree with 
        |Empty -> k Empty
        |Node(_,_,r) -> match r with
            |Empty -> k tree
            |Node(_,_,_) ->
                let root = rotleft tree in
                let v = key_of root in
                let l = root |> left in
                let r = root |> right in
                loop r (x-1) (fun right -> k (Node(v, l, right))) 
    in
    loop tree num (fun f -> f)
;;


let rec tree_to_vine = function
    |Empty -> Empty
    |Node(v,l,r) as n -> match l with 
        |Node (_,_,_) -> tree_to_vine (rotright n)
        |Empty -> Node (v, l, tree_to_vine r )
;;
let tree_to_vine' tree =
    let rec loop t k = match t with
    |Empty -> k Empty
    |Node (v,l,r) as n -> match l with
        |Node (_,_,_) -> loop (rotright n) k  
        |Empty -> loop r (fun right -> k (Node(v,l,right))) in
    loop tree (fun f -> f)
;;


let num_order tree =
    (**)
    let vine = tree_to_vine' tree in
    (**)
    let rec aux acc = function
    |Empty -> acc
    |Node (x, _, r) -> aux (x::acc) r in
    aux [] vine
;;

let rec rand_tree k = function
    |0 -> Empty 
    |n -> Node ((Random.int k), rand_tree k (n-1), rand_tree k (n-1))
;;

let rec bst_insert (k: int) (tree : int tree) = match tree with
    |Empty -> Node(k, Empty, Empty)
    |Node(x,l,r) -> if k < x 
        then Node(x, (bst_insert k l), r)
        else Node(x, l, (bst_insert k r))
;;    

let bst_ify tree =
    let rec aux root = function
    |[] -> root
    |x::xs ->
        let rt = (bst_insert x root) in 
        aux rt xs
    in
    (aux (Empty: int tree) (num_order tree))
;;

let compare x y =
    if abs (x - y) < 2 then Eq
    else if x < y      then Lt
                       else Rt
;;

let rec is_balanced = function
    | Empty -> true
    | Node(_,l,r) -> match (compare (depth l) (depth r) ) with
        |Eq -> true && (is_balanced l) && (is_balanced r)
        |_ -> false
;;


let rec balance_node = function
    |Empty -> Empty
    |Node(_,l,r) as n ->
        let dl = depth l in
        let dr = depth r in
        match compare dl dr with
        |Eq -> n
        |Lt -> n |> rotleft |> balance_node
        |Rt -> n |> rotright |> balance_node
;;

(* First attempt. Very inefficient. See below for more efficient
 * Day-Stout-Warren balancing algorithm with O(n) time complexity. *)
let rec balance = function
    |Empty -> Empty
    |node ->
        let n = balance_node node in
        let l = n |> left |> balance in
        let r = n |> right |> balance in
        let v = key_of n in
        Node (v, l, r)
;;

let mrk = print_endline (brk ^ "\n");;

let d_s_w_balance tree =
    (**)
    let cnt = count tree in
    (**) 
    let rec full_size x c = 
        if x > c then x
        else full_size (x + x + 1) c in
    (**)
    let fs = full_size 1 cnt in
    (**)
    let num_leaves = cnt - (fs/2) in
    (**)
    let rec vine_to_tree tree x = 
        if x < 1 then tree else
            vine_to_tree (compress tree x) (x/2) in
    (**)
    let vine = tree_to_vine tree in
    let first_run = compress vine num_leaves in
    vine_to_tree first_run (fs/4)
;;

let d_s_w_balance' tree =
    (**)
    let cnt = count tree in
    (**) 
    let rec full_size x c = 
        if x > c then x
        else full_size (x + x + 1) c in
    (**)
    let fs = full_size 1 cnt in
    (**)
    let num_leaves = cnt - (fs/2) in
    (**)
    let rec vine_to_tree tree x = 
        if x < 1 then tree else
            vine_to_tree (compress' tree x) (x/2) in
    (**)
    let _ = print_endline ("num_leaves = " ^ (string_of_int num_leaves)) in
    let vine = tree_to_vine' tree in
    let first_run = compress' vine num_leaves in
    vine_to_tree first_run (fs/4)
;;

(*tail recursive depth function using Continuation Passing Style *)
let depth' tree = 
    let rec depth tree k = match tree with
    |Empty -> k 0
    |Node(_,left,right) ->
        depth left (fun dleft -> depth right (fun dright -> k (1 + (max dleft dright))))
    in depth tree (fun d -> d)
;;
type ('a, 'b) cont = 
    |Kleft of 'a tree * ('a, 'b) cont (* right and k *)
    |Kright of 'b * ('a, 'b) cont  (*dleft and k*)
    |Kid
(*defunctorized version:(overly complicated and almost certainly unnecessary) *)
let depth'' tree =
    let rec depth tree k = match tree with
    |Empty -> eval k 0
    |Node(_,left,right) ->
        depth left (Kleft(right, k))
    and eval k d = match k with
        |Kleft(right, k) ->
            depth right (Kright(d, k))
        |Kright(dleft,k) ->
            eval k (1 + max d dleft)
        |Kid -> d
    in depth tree Kid
;;


let test_tree = rand_tree  500 8
let test_tree2 = rand_tree 1000 17 (*Any higher than 17 will result in stack overflow*)

let nnn = depth' test_tree2 in print_endline ("test_tree depth = " ^ (string_of_int nnn));;
let nnn = depth'' test_tree2 in print_endline ("test_tree2 depth = " ^ (string_of_int nnn));;


let n26 = Node(26, Empty, Empty)
let n18 = Node(18, n26, Empty)
let n10 = Node(10, n18, Empty)
let n23 = Node(23, Empty, Empty)
let n37 = Node(37, n10, n23)
let t3 = Node(7, n37, Empty)
let test_tree3 = bst_insert 6 (bst_insert 83 (bst_insert 76 t3))
;;
let nnn = depth' test_tree3 in print_endline ("test_tree3 depth = " ^ (string_of_int nnn));;



(*
print_endline (string_of_tree test_tree2 (string_of_int) 2);;
print_endline brk;;
*)
(*
print_endline "\nbst_insert 5 test_tree =";;
let test_tree = bst_insert 5 test_tree;;
print_endline (string_of_tree test_tree (string_of_int) 2);;
*)


print_endline (brk ^ "\n\nbst_ify test_tree...");;
let test_tree = bst_ify test_tree;;
print_endline (brk ^ "\n\nbst_ify test_tree2...");;
let test_tree2 = bst_ify test_tree2;;
print_endline (brk ^ "\n\nbst_ify test_tree3...");;
let test_tree3 = bst_ify test_tree3;;


(*
print_endline (string_of_tree test_tree2 (string_of_int) 2);;
*)

print_endline (brk ^ "\n\nbalance test_tree =");;
let test_tree' = d_s_w_balance test_tree;;
print_endline (brk ^ "\n\nbalance test_tree2 =");;
let test_tree2' = d_s_w_balance test_tree2;;
print_endline (brk ^ "\n\nbalance test_tree3 =");;
let test_tree3' = d_s_w_balance test_tree3;;


print_endline (brk ^ "\n\nbalance' test_tree =");;
let test_tree = d_s_w_balance' test_tree;;
print_endline (brk ^ "\n\nbalance' test_tree2 =");;
let test_tree2 = d_s_w_balance' test_tree2;;
print_endline (brk ^ "\n\nbalance' test_tree3 =");;
let test_tree3 = d_s_w_balance' test_tree3;;

(*
print_endline (string_of_tree test_tree2 (string_of_int) 2);;
*)

print_endline ("Balance Successful test_tree  = " ^ string_of_bool (is_balanced test_tree'));;
print_endline ("Balance Successful test_tree2 = " ^ string_of_bool (is_balanced test_tree2'));;
print_endline ("Balance Successful test_tree3 = " ^ string_of_bool (is_balanced test_tree3'));;















