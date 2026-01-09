(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal

let num_bits = 32

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; dial_amt : 'a [@bits num_bits]
    ; is_left : 'a
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ending_zero_count : 'a With_valid.t [@bits num_bits]
    ; passing_zero_count : 'a With_valid.t [@bits num_bits]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module Divider = Hardcaml_circuits.Divide_by_constant.Make (Signal)

let create
  scope
  ({ clock; clear; start; finish; dial_amt; is_left; data_in_valid } : _ I.t)
  : _ O.t
  =
  (*setup*)
  let sized_int x = of_unsigned_int ~width:num_bits x in
  let change_repr x = mux2 (x ==: sized_int 0) (sized_int 0) (sized_int 100 -: x) in
  let choose_repr x do_change = mux2 do_change (change_repr x) x in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var cur_dial = Variable.reg spec ~width:num_bits in
  (* how many times cur_dial was exactly 0*)
  let%hw_var ending_zeros = Variable.reg spec ~width:num_bits in
  (* how many times cur_dial passed by 0 *)
  let%hw_var passing_zeros = Variable.reg spec ~width:num_bits in
  (* intermediates *)
  let repr = Variable.wire ~default:(zero num_bits) () in
  let sum = Variable.wire ~default:(zero num_bits) () in
  let quot = Variable.wire ~default:(zero num_bits) () in
  let rem = Variable.wire ~default:(zero num_bits) () in
  let next_pos = Variable.wire ~default:(zero num_bits) () in
  (* output wires *)
  let ending_zero_count = Variable.wire ~default:(zero num_bits) () in
  let passing_zero_count = Variable.wire ~default:(zero num_bits) () in
  let valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ cur_dial <-- sized_int 50
                ; ending_zeros <-- sized_int 0
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ (* convert to the right representation (+ or -) depending on direction *)
                  repr <-- choose_repr cur_dial.value is_left
                  (* calculate how many times we pass zero and what we end up as *)
                ; sum <-- repr.value +: dial_amt
                ; quot
                  <-- uresize
                        ~width:num_bits
                        (Divider.divide ~divisor:(Bigint.of_int 100) sum.value)
                ; rem
                  <-- sum.value -: uresize ~width:num_bits (quot.value *+ sized_int 100)
                ; next_pos <-- choose_repr rem.value is_left
                  (* go to next position and update counts *)
                ; cur_dial <-- next_pos.value
                ; passing_zeros <-- passing_zeros.value +: quot.value
                ; when_
                    (next_pos.value ==: sized_int 0)
                    [ ending_zeros <-- ending_zeros.value +: sized_int 1 ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; ( Done
          , [ ending_zero_count <-- ending_zeros.value
            ; passing_zero_count <-- passing_zeros.value
            ; valid <-- vdd
            ; when_ finish [ sm.set_next Accepting_inputs ]
            ] )
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { ending_zero_count = { value = ending_zero_count.value; valid = valid.value }
  ; passing_zero_count = { value = passing_zero_count.value; valid = valid.value }
  }
;;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;
