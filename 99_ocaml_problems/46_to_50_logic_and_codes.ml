


(* Let us define a small 'language' for boolean expressions containing variables *)

type bool_expr =
    |Var of string
    |Not of bool_expr
    |And of bool_expr * bool_expr
    |Or  of bool_expr * bool_expr
;;

(* A logical expression in two variables can then be written in prefix notation

    For example (a v b) ^ (a ^ b) is written:                   
*)
And(Or(Var "a", Var "b")),(And (Var "a",Var "b"));;

(* 46. & 47. Truth tables for logical expressions  *)


let rec table2 a b exp =
    let rec aux a a_val b b_val = function
    | Var(v) -> if v = a then a_val else
                if v = b then b_val else
                failwith "table_2-unkown_value"
    | Not(exp) -> not (aux a a_val b b_val exp)
    | And(exp1, exp2) -> aux a a_val b b_val exp1 
                        && aux a a_val b b_val exp2
    | Or(exp1, exp2) -> aux a a_val b b_val exp1 
                        || aux a a_val b b_val exp2
    in
    [
    (true, true, aux a true b true exp);
    (true, false, aux a true b false exp);
    (false, true, aux a false b true exp);
    (false, false, aux a false b false exp)
    ]
;;


(* 48. Truth tables for logical expressions - general version *)

let table variables expr =
    let rec inputs = function
    |[] -> [[]]
    |hd::tl -> let ls = inputs tl in
                (List.map (fun ls -> (hd,true)::ls) ls )
                @ (List.map (fun ls -> (hd,false)::ls) ls)
    in
    let ins = inputs variables
    in
    let rec lookup k = function
    |[] -> failwith "table_lookup-unkown_variable"
    |(k',v)::tl -> if k = k' then v else lookup k tl
    in
    let rec result row = function
    |Var(v) -> lookup v row
    |Not(exp) -> not (result row exp)
    |And(exp1, exp2) -> result row exp1 && result row exp2
    |Or(exp1, exp2) -> result row exp1 || result row exp2
    in
    List.map (fun ls -> (ls, result ls expr)) ins
;;

(* ocaml.org solution: (very similar in practice) 
(* [val_vars] is an associative list containing the truth value of
     each variable.  For efficiency, a Map or a Hashtlb should be
     preferred. *)
  
  let rec eval val_vars = function
    | Var x -> List.assoc x val_vars
    | Not e -> not(eval val_vars e)
    | And(e1, e2) -> eval val_vars e1 && eval val_vars e2
    | Or(e1, e2) -> eval val_vars e1 || eval val_vars e2
  
  (* Again, this is an easy and short implementation rather than an
     efficient one. *)
  let rec table_make val_vars vars expr =
    match vars with
    | [] -> [(List.rev val_vars, eval val_vars expr)]
    | v :: tl ->
       table_make ((v, true) :: val_vars) tl expr
       @ table_make ((v, false) :: val_vars) tl expr
  
  let table vars expr = table_make [] vars expr;;
val eval : (string * bool) list -> bool_expr -> bool = <fun>
val table_make :
  (string * bool) list ->
  string list -> bool_expr -> ((string * bool) list * bool) list = <fun>
val table : string list -> bool_expr -> ((string * bool) list * bool) list =
  <fun>
*)

(* 49. Gray code   *)


let rec gray = function
    |1 -> ["0"; "1"]
    |n -> let ls = gray (n-1) in
            (List.map (fun s -> "0"^s) ls)
            @ (List.map (fun s -> "1"^s) (List.rev ls))
;;

(* ocaml.org solution:   *)
let prepend c s =
    (* Prepend the char [c] to the string [s]. *)
    let s' = String.create (String.length s + 1) in
    s'.[0] <- c;
    String.blit s 0 s' 1 (String.length s);
    s'
  
  let rec gray' n =
    if n <= 1 then ["0"; "1"]
    else let g = gray (n - 1) in
         List.map (prepend '0') g @ List.rev_map (prepend '1') g;;



(* 50. Huffman coding   *)

type 'a leftist = Node of 'a * 'a leftist * 'a leftist * int | Empty

let singleton k = Node (k, Empty, Empty, 1)

let rank = function Empty -> 0 | Node(_,_,_,h) -> h

let (<-<) (t1:(bytes * int) leftist) (t2:(bytes*int) leftist) : bool =
    match t1,t2 with
    |Node((_,f),_,_,_),Node((_,f'),_,_,_) -> f < f'
    |_,_ -> failwith "<-<"

let rec merge (t1:((bytes * int) leftist) leftist) (t2:((bytes*int) leftist) leftist) 
    :(((bytes * int) leftist) leftist) =
    match t1,t2 with
    |Empty,t|t,Empty -> t
    |Node(v,l,r,_),Node(v',_,_,_) ->
        if v' <-< v then merge t2 t1 else
        let merged = merge r t2 in
        let rank_left = rank l in let rank_right = rank merged in
        if rank_left < rank_right then Node(v, merged, l, rank_left+1)
        else Node(v, l, merged, rank_right+1)

let insert x t = merge (singleton x) t

let get_min = function
    |Empty -> failwith "get_min->empty"
    |Node (v,_,_,_) -> v

let delete_min = function
    |Empty -> failwith "delete_min->empty"
    |Node(_,l,r,_) -> merge l r

let combine (t1:(bytes * int) leftist) (t2:(bytes*int) leftist) : ((bytes * int) leftist) =
    match t1,t2 with
    |Empty,t|t,Empty -> t
    |Node((_,v),_,_,_),Node((_,v'),_,_,h) ->
        Node( ("",(v+v')), t1, t2, (h+1) ) 

let huffman freqs : ((bytes * bytes) list ) = 
    let tree = List.fold_left (fun tree pair ->insert (Node(pair,Empty,Empty,1)) tree) Empty freqs in
    let rec build_huffman = function
    |Empty -> failwith "huffman aux"
    |Node(internal_tree,Empty,Empty,_) -> internal_tree
    |Node(k,l,r,_)as t-> 
        let min1 = get_min t in
        let t' = delete_min t in
        let min2 = get_min t' in
        let new_t  = combine min1 min2 in
        build_huffman (merge (singleton new_t) (delete_min t'))
    in
    let t = build_huffman tree
    in
    let rec aux code = function
    |Empty -> []
    |Node((s,f),l,r,_) -> 
        if s <> "" then [(s,code)] else
        (aux (code ^ "0") l) @ (aux (code ^ "1") r)
    in
    aux "" t

(* ocaml.org solution:   *)

(* Simple priority queue where the priorities are integers 0..100.
     The node with the lowest probability comes first. *)
  module Pq = struct
    type 'a t = { data: 'a list array;  mutable first: int }
    let make() = { data = Array.make 101 [];  first = 101 }
  
    let add q p x =
      q.data.(p) <- x :: q.data.(p);  q.first <- min p q.first
  
    let get_min q =
      if q.first = 101 then None
      else
        match q.data.(q.first) with
        | [] -> assert false
        | x :: tl ->
           let p = q.first in
           q.data.(q.first) <- tl;
           while q.first < 101 && q.data.(q.first) = [] do
             q.first <- q.first + 1
           done;
           Some(p, x)
  end
  
  type tree =
    | Leaf of string
    | Node of tree * tree
  
  let rec huffman_tree q =
    match Pq.get_min q, Pq.get_min q with
    | Some(p1, t1), Some(p2, t2) -> Pq.add q (p1 + p2) (Node(t1, t2));
                                   huffman_tree q
    | Some(_, t), None | None, Some(_, t) -> t
    | None, None -> assert false
  
  (* Build the prefix-free binary code from the tree *)
  let rec prefixes_of_tree prefix = function
    | Leaf s -> [(s, prefix)]
    | Node(t0, t1) -> prefixes_of_tree (prefix ^ "0") t0
                     @ prefixes_of_tree (prefix ^ "1") t1
  
  let huffman' fs =
    if List.fold_left (fun s (_,p) -> s + p) 0 fs <> 100 then
      failwith "huffman: sum of weights must be 100";
    let q = Pq.make() in
    List.iter (fun (s,f) -> Pq.add q f (Leaf s)) fs;
    prefixes_of_tree "" (huffman_tree q);;

module Pq :
  sig
    type 'a t = { data : 'a list array; mutable first : int; }
    val make : unit -> 'a t
    val add : 'a t -> int -> 'a -> unit
    val get_min : 'a t -> (int * 'a) option
  end
    

(* create random freq list for testing: *)

let rec rand_fs k =
    let rand_chr () = (Char.chr (97 + (Random.int 26))) 
    in
    let r () = (Char.escaped (rand_chr ()))
    in
    let rec aux acc = function
    |0 -> acc
    |k -> let v = (r ()) ^ (r ()) ^ (r ()) ^ (r ())
            in aux ((v, Random.int 1000):: acc) (k-1)
    in
    aux [] k



        
    

























