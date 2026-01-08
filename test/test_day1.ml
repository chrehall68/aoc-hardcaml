open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day1 = Aoc_hardcaml.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

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
  print_s [%message "Result" (zero_count : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
let waves_config = Waves_config.no_waves

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let test1 = [ 16, "L"; 26, "R"; 60, "L"; 30, "L"; 100, "R"; 30, "R" ]

let%expect_test "test1" =
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical (simple_testbench test1);
  [%expect {| (Result (zero_count 2)) |}]
;;

let%expect_test "test1 waveforms" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    (simple_testbench test1);
  [%expect
    {|
    (Result (zero_count 2))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │                            ││────────────┬───┬───────┬───────┬───────┬───────────────┬───│
    │day1$cur_dial               ││ 0          │50 │34     │60     │0      │70             │0  │
    │                            ││────────────┴───┴───────┴───────┴───────┴───────────────┴───│
    │day1$i$clear                ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │day1$i$data_in_valid        ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────│
    │day1$i$dial_amt             ││ 0          │16     │26     │60     │30     │100    │30     │
    │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────│
    │day1$i$finish               ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$i$is_left              ││            ┌───────┐       ┌───────────────┐               │
    │                            ││────────────┘       └───────┘               └───────────────│
    │day1$i$start                ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │day1$o$zero_count$valid     ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$zero_count$value     ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────┬───────────────────────┬───│
    │day1$zero_counter           ││ 0                              │1                      │2  │
    │                            ││────────────────────────────────┴───────────────────────┴───│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;

let test2 = [ 23, "R"; 27, "R"; 50, "L"; 50, "L"; 100, "R"; 70, "R"; 30, "R" ]

let%expect_test "test2" =
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical (simple_testbench test2);
  [%expect {| (Result (zero_count 4)) |}]
;;

(* larger numbers... *)
let test3 =
  [ 1050, "R"; 730, "L"; 123430, "R"; 2204, "L"; 1294, "L"; 1010, "R"; 1012, "L" ]
;;

let%expect_test "test3" =
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical (simple_testbench test3);
  [%expect {| (Result (zero_count 3)) |}]
;;

let%expect_test "test3 waves" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:120
        ~wave_width:1
        waves)
    (simple_testbench test3);
  [%expect
    {|
    (Result (zero_count 3))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │                            ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────    │
    │day1$cur_dial               ││ 0          │50 │0      │70     │0      │96     │2      │12     │0                      │
    │                            ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────    │
    │day1$i$clear                ││────┐                                                                                   │
    │                            ││    └───────────────────────────────────────────────────────────────────────────────    │
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │day1$i$data_in_valid        ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐                       │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───────────────────    │
    │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────────    │
    │day1$i$dial_amt             ││ 0          │1050   │730    │123430 │2204   │1294   │1010   │1012                       │
    │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────────    │
    │day1$i$finish               ││                                                                    ┌───┐               │
    │                            ││────────────────────────────────────────────────────────────────────┘   └───────────    │
    │day1$i$is_left              ││                    ┌───────┐       ┌───────────────┐       ┌───────────────────────    │
    │                            ││────────────────────┘       └───────┘               └───────┘                           │
    │day1$i$start                ││        ┌───┐                                                                           │
    │                            ││────────┘   └───────────────────────────────────────────────────────────────────────    │
    │day1$o$zero_count$valid     ││                                                                        ┌───────────    │
    │                            ││────────────────────────────────────────────────────────────────────────┘               │
    │                            ││────────────────────────────────────────────────────────────────────────┬───────────    │
    │day1$o$zero_count$value     ││ 0                                                                      │3              │
    │                            ││────────────────────────────────────────────────────────────────────────┴───────────    │
    │                            ││────────────────┬───────────────┬───────────────────────────────┬───────────────────    │
    │day1$zero_counter           ││ 0              │1              │2                              │3                      │
    │                            ││────────────────┴───────────────┴───────────────────────────────┴───────────────────    │
    └────────────────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
