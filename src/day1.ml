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
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      zero_count : 'a With_valid.t [@bits num_bits]
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

let create
  scope
  ({ clock; clear; start; finish; dial_amt; is_left; data_in_valid } : _ I.t)
  : _ O.t
  =
  let sized_int x = of_unsigned_int ~width:num_bits x in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var cur_dial = Variable.reg spec ~width:num_bits in
  let%hw_var zero_counter = Variable.reg spec ~width:num_bits in
  (* We don't need to name the range here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
  let addition_result = Variable.wire ~default:(zero num_bits) () in
  let subtraction_result = Variable.wire ~default:(zero num_bits) () in
  let negated_subtraction_result = Variable.wire ~default:(zero num_bits) () in
  let mod_result = Variable.wire ~default:(zero num_bits) () in
  let signed_mod_result = Variable.wire ~default:(zero num_bits) () in
  let zero_count = Variable.wire ~default:(zero num_bits) () in
  let zero_count_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ cur_dial <-- sized_int 50
                ; zero_counter <-- sized_int 0
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ addition_result <-- cur_dial.value +: dial_amt
                ; subtraction_result <-- cur_dial.value -: dial_amt
                  (* convert from negative to positive means flip all bits and add 1 *)
                ; negated_subtraction_result <-- ~:(subtraction_result.value) +:. 1
                ; if_
                    is_left
                    [ if_
                        (subtraction_result.value <+ sized_int 0)
                        [ (* then we should mod the negated subtraction result and then re-negate the result*)
                          mod_result
                          <-- uresize
                                ~width:num_bits
                                (Hardcaml_circuits.Modulo.unsigned_by_constant
                                   (module Signal)
                                   negated_subtraction_result.value
                                   100)
                        ; signed_mod_result <-- sized_int 100 -: mod_result.value
                        ]
                        [ (* then we want have no overflow *)
                          mod_result <-- subtraction_result.value
                        ; signed_mod_result <-- mod_result.value
                        ]
                    ]
                    [ (* need to move the dial right*)
                      if_
                        (addition_result.value >=+ sized_int 100)
                        [ (* then we need to do modulo*)
                          mod_result
                          <-- uresize
                                ~width:num_bits
                                (Hardcaml_circuits.Modulo.unsigned_by_constant
                                   (module Signal)
                                   addition_result.value
                                   100)
                        ; signed_mod_result <-- mod_result.value
                        ]
                        [ (* then we want have no overflow *)
                          mod_result <-- addition_result.value
                        ; signed_mod_result <-- mod_result.value
                        ]
                    ]
                ; cur_dial <-- signed_mod_result.value
                ; when_
                    (signed_mod_result.value ==: sized_int 0)
                    [ zero_counter <-- zero_counter.value +: sized_int 1 ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; ( Done
          , [ zero_count <-- zero_counter.value
            ; zero_count_valid <-- vdd
            ; when_ finish [ sm.set_next Accepting_inputs ]
            ] )
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { zero_count = { value = zero_count.value; valid = zero_count_valid.value } }
;;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;
