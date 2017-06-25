
(* 
 *    Single-File Programs
 * A utility that reads lines from stdin and computes a frequency count of the lines. 
 * At the end, the 10 lines with the highest frequency counts are written out. *)
open Core.Std

let assoc = [("one", 1); ("two", 2); ("three", 3)] ;;
let some2 = List.Assoc.find assoc "two" ;;
List.Assoc.add assoc "four" 4 (* add a new key *) ;;
List.Assoc.add assoc "two" 4 (* overwrite and existing key *) ;;

(* freq.ml:                         *)

let build_counts () =
    In_channel.fold_lines stdin ~init:[] ~f:(fun counts line ->
        let count = 
            match List.Assoc.find counts line with
            |None -> 0
            |Some x -> x
        in
        List.Assoc.add counts line (count + 1)
    )
;;

let () =
    build_counts ()
    |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
    |> (fun l -> List.take l 10)
    |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)

(*
 *
 * Mulitfile Programs and Modules
 *
 * Source files in OCaml are tied to the module system.
 * Each file compiles down to a module whose name is 
 * derived from the name of the file.
 *
 * At the moment we are using an association list but this
 * isn't a very efficient data structure with the O(n2) time
 * complexity for processing.
 *
 * We can fix this by replacing that part of freq.ml with 
 * functionality that we will define in a seperate module 'counter.ml'.
 * Module names are capitalized even if the file is not. So our module will
 * be called Counter.
 *)
(* New Code:    *)
open Counter

let build_counts () =
 In_channel.fold_lines stdin ~init:Counter.empty ~f:Counter.touch  
        (*ocamlbuild will realize that counter.ml needs to be compiled too *)
let () =
    build_counts ()
    |> Counter.to_list
    |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
    |> (fun l -> List.take l 10)
    |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)


(* Signatures and Abstract Types  
 * 
 * Some code in freq.ml still depends on the details of Counter. The definition
 * of build_counts depends on the fact that the empty set of frequency counts is
 * represented as an empty list. We'd ideally like to prevent this kind of dependency
 * so that we can change the implementation of Counter without needing to change client
 * code in freq.ml.

 * A modules details can be hidden by attaching an interface. (Note that in OCaml interface/
 * signature/ 'module type' are used interchangeably) A module defined by file.ml can be constrained
 * by a file called file.mli.

 * For counter.mli we'll start by writing down an interface that describes the current state:
 *  val touch : (string * int) list -> string -> (string * int) list

 * then we'll change it so that the collection of frequency counts is abstract allowing us to change our
 * implementation at will. To do this we'll need to also define a 'empty' function and a 'to_list' function:
        
        type t
        
        val empty : t

        val touch : t -> string -> t

        val to_list : t -> (string * int) list

 * Now we can change out Counter implementation to use a more efficient map structure without 
 * having to change this file.
 *)















