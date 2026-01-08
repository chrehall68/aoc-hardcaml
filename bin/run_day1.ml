open! Core
open! Hardcaml
open! Hardcaml_test_harness
module Day1 = Aoc_hardcaml.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)
open! Stdio.In_channel

let ( <--. ) = Bits.( <--. )

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
  while not (Bits.to_bool !(outputs.zero_count.valid)) do
    cycle ()
  done;
  let zero_count = Bits.to_unsigned_int !(outputs.zero_count.value) in
  print_s [%message "Result" (zero_count : int)]
;;

let () =
  (* Read in the input values from stdin *)
  let inputs = ref [] in
  let line = ref (input_line stdin) in
  let continue = ref (is_some !line) in
  while !continue do
    match !line with
    | None -> continue := false
    | Some s ->
      inputs := s :: !inputs;
      line := input_line stdin
  done;
  (* turn them into tuples that our testbench expects *)
  let input_values = List.rev !inputs in
  let input_tuples =
    List.map input_values ~f:(fun s ->
      ( int_of_string (String.sub s ~pos:1 ~len:(String.length s - 1))
      , String.sub s ~pos:0 ~len:1 ))
  in
  (* run testbench *)
  let waves_config = Waves_config.no_waves in
  Harness.run_advanced
    ~waves_config
    ~create:Day1.hierarchical
    (simple_testbench input_tuples)
;;
