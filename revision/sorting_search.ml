


let rec insert x = function
    |[] -> [x]
    |y::ys -> 
        if x < y then x :: y :: ys else y :: insert x ys
    ;;

let rec insert_sort acc = function
    | [] -> acc
    | x::xs -> insert_sort (insert x acc) xs
;;

let partition p ls =
    let rec partition' p (yes,nos) = function 
    |[] -> (yes,nos)
    |x::xs -> if p x then partition' p (x::yes, nos) xs
                else partition' p (yes, x::nos) xs
    in partition' p ([],[]) ls
    ;;

let over_3 = function | x -> x > 3;; 

let rec merge = function
    |[],[] -> []
    | x,[] -> x
    | [],y -> y
    | x::xs, y::ys ->
        if x < y then x :: (merge (xs,y::ys))
        else y :: merge ((x::xs, ys))
    ;;



let rec quicksort = function
    |[] -> []
    |x::xs -> 
        let (yes,nos) = partition (fun a -> a > x) xs in
        let ls = quicksort yes in
        let rs = quicksort nos in
        merge (x::ls,rs) 
    ;;

let rec quicksort' = function
    |[] -> []
    |x::xs -> 
        let smaller,greater = partition (fun a -> a < x) xs in
        (quicksort' smaller) @ [x] @ (quicksort' greater)
    ;;       
        

 
let a = insert_sort [] [1;2;334;323;3454562;234;43;5;67;2];;
let b = partition over_3 [3;4;3;2;344;3234];;
let c = merge ([2;3;4;5],[1;2]);;
let d = quicksort [2;4;5;2;4;5;3;4;5;6;3;4;5;6;3;35;6;3;43;45;3456;34;53;46;3];;





 
