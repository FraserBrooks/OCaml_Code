


(* 31. Determine whether a given integer number is prime. *)

let is_prime n = 
    let rec fill acc = function
    |1 -> acc
    |k -> fill (k::acc) (k-1)
    in
    let ls = fill [] n
    in
    let rec rm_multiple acc k x = function
    | [] -> acc
    | h::tl -> if h <> x then
                    ( 
                    if h > x then 
                        rm_multiple (h::acc) k (x + k) tl 
                    else 
                        rm_multiple (h::acc) k x tl
                    ) 
                else 
                    rm_multiple acc k (x + k) tl
    in
    let rec aux = function
    |[]-> false
    |[k] -> k = n && n <> 1 
    |x::xs -> aux (List.rev ((rm_multiple [] x (x + x) xs)))
    in
    aux ls
;;
(* It would appear that the above is very much not the way to implement
    Sieve of Eratosthenes in ocaml...               *)

(* ocaml.org solution: *)
(* Recall that d divides n iff n mod d = 0. This is a naive solution. 
   See the Sieve of Eratosthenes for a more clever one. *)

let is_prime' n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2;;


(* 32. Determine the greatest common divisor of two positive integer numbers  *)

let rec gcd x y =
    if x = 0 then y else if y = 0 then x else
    let z = (max x y) - (min x y) in
    gcd (min x y) z;;

(* ocaml.org simpler solution:  *)
let rec gcd' x y = 
    if x = 0 then y else gcd' (y mod x) x;; 

(* 33. Determine whether two positive integer numbers are coprime  *)

let rec gcd' x y = 
    if x = 0 then y else gcd' (y mod x) x;; 

let coprime x y = gcd' x y = 1;;


(* 34. Calculate Euler's totient function φ#(m)   *)

let phi m =
    let rec aux count = function
    |0 -> count
    |k -> if coprime k m then aux (count+1) (k-1)
            else aux count (k-1)
    in
    aux 0 m
;;

(* ocaml.org alternative:  *)
let phi n =
    let rec count_coprime acc d =
      if d < n then
        count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
      else acc
    in
    if n = 1 then 1 else count_coprime 0 1;;    
    
(* 35. Determine the prime factors of a given positive integer.  *)

let rec factors n = 
    let rec aux acc a b =
    if a = 1 then acc else
    if a mod b = 0 then aux (b::acc) (a/b) b 
    else aux acc a (b+1)
    in
    List.rev (aux [] n 2)
;;


(* 36. Determine the prime factors of a given positive integer (2) (with multiplicity) *)

let factors_mul n =
    let rec factors n = 
        let rec aux acc a b =
        if a = 1 then acc else
        if a mod b = 0 then aux (b::acc) (a/b) b 
        else aux acc a (b+1)
        in
        List.rev (aux [] n 2)
    in
    let ls = factors n 
    in
    let rec aux acc count last = function
    |[] -> ((last, count)::acc)
    |x::xs -> if x = last then aux acc (count + 1) last xs
                else aux ((last, count)::acc) 1 x xs
    in
    match ls with   
    |[] -> []
    |h::tl -> List.rev (aux [] 1 h tl)
;;

(* ocaml.org terser solution:  *)
let factors_mul' n =
    let rec aux d n =
      if n = 1 then [] else
        if n mod d = 0 then
          match aux d (n / d) with
          | (h,n) :: t when h = d -> (h,n+1) :: t
          | l -> (d,1) :: l
        else aux (d+1) n
    in
    aux 2 n;;

(* 37. Calculate Euler's totient function #(m) IMPROVED  *)

let phi_improved n =
    let rec pow n p = if p < 1 then 1 else n * (pow n (p-1))
    in
    let ls = factors_mul n 
    in
    let rec aux acc = function
    |[] -> acc
    |(p,m)::tl -> aux ( (p-1) * (pow (p) (m-1) ) * acc) tl
    in
    aux 1 ls
;;


(* 38. Compare the two methods of calculating Euler's totient function  *)

let timeit foo x =
    let t0 = Unix.gettimeofday() in
    ignore(foo x);
    let t1 = Unix.gettimeofday() in
    t1 -. t0
;;

(* 39. A list of prime numbers   *)

let all_primes n m =
    let rec fill acc = function
    |k when k > m -> acc
    |1 -> fill acc 2 (*1 is not prime *)
    |k -> fill ((k,true)::acc) (k+1)
    in
    let ls = List.rev (fill [] n)
    in
    let rec pass acc k x = function
    |[] -> acc
    |((p,prime)::tl as ls) ->
        let z = if p > x then (x + k) else x 
        in
        if not prime || p = x then 
            pass ((p,false)::acc) k z tl  
        else if p > x then
            pass acc k z ls
        else
            pass ((p, true)::acc) k z tl
    in
    let rec mark_primes ls = function
    | n when n > m/2 -> ls
    | n -> mark_primes (List.rev (pass [] n (n+n) ls)) (n + 1)
    in
    List.rev(
        List.fold_left (fun acc (n,p) -> if p then n::acc else acc) [] (mark_primes ls 2)
    )
;;

(* ocaml.org much simpler/faster solution: *)
let rec all_primes' a b =
 (*   let is_prime n =
        let n = max n (-n) in
        let rec is_not_divisor d =
            d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
        is_not_divisor 2
    in*)
    if a > b then [] else
      let rest = all_primes' (a + 1) b in
      if is_prime a then a :: rest else rest
;;


(* 40. Goldbach's Conjecture    *)

let goldbach n =
    let is_prime a =
        let a = max a (-a) in
        let rec aux b =
        b * b > a || (a mod b <> 0 && aux (b+1)) in
        aux 2
    in
    let rec all_primes a lim =
    if a > lim then [] else
        let rest = all_primes (a + 1) lim in
        if is_prime a then a :: rest else rest
    in
    let primes = all_primes 2 n 
    in
    let rec aux acc = function
        |[] -> acc
        |h::tl ->
            if h * 2 > n then acc else
            if is_prime (n - h) then aux ((h,n-h)::acc) tl
            else aux acc tl
    in
    aux [] primes
;;
(* val goldbach: : int -> (int * int) list = <fun>   *)

(* The solution given on ocaml.org returns the goldbach pair with
    the smallest n where n is the min of the two prime numbers.
    The solution above returns a list of all possible goldbach pairs. *)

(* 41. Goldbach's list  *)

let goldbach_list n lim =
    let rec aux acc n =
        if n > lim then acc else
        if n mod 2 <> 0 then aux acc (n+1) else
        let g = goldbach n in
        aux ((n,g)::acc) (n+2)
    in
    aux [] n
;;
(* val goldbach_list : int -> int -> (int * (int * int) list) list = <fun> *)

(* 42. Goldbach's list with min prime *)

let goldbach_limit n lim min_p =
    let ls = goldbach_list n lim in
    List.fold_left (fun acc (p, ps) ->
                let ps = List.filter (fun (a,b) -> (min a b) > min_p) ps
                in
                if ps = [] then acc else (p, ps)::acc
             ) [] ls
;;
(* val goldbach_limit : int -> int -> int -> (int * (int * int) list) list = <fun> *)
  
(* The below is a modification to my implementation above
    to check it corresponds to the returned values of the 
    solution given on ocaml.org (reproduced below)

let rec check_last m = function
    |[] -> None
    |[(a,b) as p] -> if (min a b) > m then Some p else None
    |_::tl -> check_last m tl 
;;
let goldbach_limit'' n lim min_p =
    let ls = goldbach_list n lim in
    List.fold_left (fun acc (p, ps) ->
                match (check_last min_p ps) with
                |None -> acc
                |Some x -> (p, x)::acc 
             ) [] ls  ;;  
*)

(* ocaml.org solution:  *)
let goldbach' n =
    let rec aux d =
      if is_prime d && is_prime (n - d) then (d, n-d)
      else aux (d+1) in
    aux 2;;

let rec goldbach_list' a b =
    if a > b then [] else
      if a mod 2 = 1 then goldbach_list' (a+1) b
      else (a, goldbach' a) :: goldbach_list' (a+2) b;;
  
  let goldbach_limit' a b lim =
    List.filter (fun (_,(a,b)) -> a > lim && b > lim) (goldbach_list' a b);;

(* end of arithmetic section of 99ocamlproblems  *)













