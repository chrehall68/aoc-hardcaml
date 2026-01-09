open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day1 = Aoc_hardcaml.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )

type result =
  { ending_zero_count : int
  ; passing_zero_count : int
  }

let simple_testbench (input_values : (int * string) list) (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n dir =
    inputs.dial_amt <--. n;
    inputs.is_left := if equal_string dir "L" then Bits.vdd else Bits.gnd;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter input_values ~f:(fun (x, dir) -> feed_input x dir);
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.valid)) do
    cycle ()
  done;
  let ending_zero_count = Bits.to_unsigned_int !(outputs.ending_zero_count) in
  let passing_zero_count = Bits.to_unsigned_int !(outputs.passing_zero_count) in
  { ending_zero_count; passing_zero_count }
;;

let run_advanced ?waves_config ?trace ?print_waves_after_test x =
  Harness.run_advanced
    ?waves_config
    ?trace
    ?print_waves_after_test
    ~create:Day1.hierarchical
    (simple_testbench x)
;;
