@@ portable

open! Core
open! Import

module Request : sig
  type t =
    { label : String.Utf8.t (** Only used for error messages *)
    ; desired_width : int
    ; min_width : int or_null (** If Null, we'll always guarantee width >=1. *)
    }
end

(** Computes the width we should use for each column given these desiderata. Does not
    account for spacing or delimiters at all *)
val compute : max_width:int -> Request.t list -> int list
