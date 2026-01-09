open! Core
open! Hardcaml

val num_bits : int

(* every clock cycle, we will take a new "line" of input
where a "line" of input should be the direction (L/R, encoded as low or high)
and an integer (although the AOC input is only up to 100, I allow up to 32 bits integers
since my solution doesn't depend that much on the magnitude of the numbers)

We'll also take start, finish, and clear.
- start should make the state machine start accepting inputs
- finish should make us output our results
- clear should reset the state machine*)
module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; dial_amt : 'a
    ; is_left : 'a
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { ending_zero_count : 'a
    ; passing_zero_count : 'a
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
