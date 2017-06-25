
(* Nested Modules  
 * Modules can be nested inside eachother: *)

open Core.Std

module Username' : sig
    type t
    val of_string : string -> t
    val to_string : t -> string
end = struct
    type t = string
    let of_string x = x
    let to_string x = x
end

module Hostname' : sig
    type t
    val of_string : string -> t
    val to_string : t -> string
end = struct
    type t = string
    let of_string x = x
    let to_string x = x
end

(* The code above defines two abstract types that we can mint
 * for unique identifiers that we might otherwise get confused 

 * We can write the declaration slightly differently by giving
 * the signature its own top-level 'module type' declaration.
 * This allows the creation of multiple distinct types with the
 * same underlying implementation in a lightweight way.     *)

module type ID = sig
    type t
    val of_string : string -> string
    val to_string : string -> string
end
module String_id = struct
    type t = string
    let of_string x = x
    let to_string x = x
end

module Username : ID = String_id
module Hostname : ID = String_id

type session_info = { user: Username.t;
                      host: Hostname.t;
                      when_started: Time.t;
                    }

let sessions_have_same_user s1 s2 =
    s1.user = s2.user








