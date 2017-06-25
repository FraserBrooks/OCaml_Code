
open Core.Std

(** Bump the frequency count for the given string. *)

(*
val touch : (string * int) list -> string -> (string * int) list
*)

(* This is a start but we want to hide the fact that frequency counts are 
 * represented as association list. We can do this by making the type of
 * frequency counts 'abstract'.
*)

(** A collection of string frequency counts. *)
type t

(** The empty set of frequency counts.  *)
val empty : t

(** Bump the frequency count for the given string. *)
val touch : t -> string -> t

(** Converts the set of frequency counts to an association list. A string shows
    up at most once, and the counts are >= 1. *)
val to_list : t -> (string * int) list

(*  Note that we needed to add empty and to_list otherwise there would be no way
    to create a Counter.t or get data out of one.

 *  We have also done some documentation. The .mli file is a natural place to store
    documentation. Comments started with a double ** causes them to be picked up by the
    ocamldoc tool when generating api documentation.        *)

(* Concrete Types in signatures: For when you want to expose more detail to client code *)

(** Represents the median computed from a set of strings. *)
type median = |Median of string | Before_and_after of string * string

val median : t -> median






