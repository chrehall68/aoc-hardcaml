open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let test1 = [ 16, "L"; 26, "R"; 60, "L"; 30, "L"; 100, "R"; 30, "R" ]

let printer (result : Testbenches.Day1.result) =
  print_s
    [%message "Result" (result.ending_zero_count : int) (result.passing_zero_count : int)]
;;

let%expect_test "test1" =
  printer (Testbenches.Day1.run_advanced ~waves_config:Waves_config.no_waves test1);
  [%expect {| (Result (result.ending_zero_count 2) (result.passing_zero_count 3)) |}]
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
  printer
    (Testbenches.Day1.run_advanced
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
       test1);
  [%expect
    {|
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │                            ││────────────┬───┬───────┬───────┬───────┬───────────────┬───│
    │day1$cur_dial               ││ 0          │50 │34     │60     │0      │70             │0  │
    │                            ││────────────┴───┴───────┴───────┴───────┴───────────────┴───│
    │                            ││────────────────────────────────┬───────────────────────┬───│
    │day1$ending_zeros           ││ 0                              │1                      │2  │
    │                            ││────────────────────────────────┴───────────────────────┴───│
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
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$ending_zero_count    ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$passing_zero_count   ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$valid                ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────┬───────────────┬───────┬───│
    │day1$passing_zeros          ││ 0                              │1              │2      │3  │
    │                            ││────────────────────────────────┴───────────────┴───────┴───│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    (Result (result.ending_zero_count 2) (result.passing_zero_count 3))
    |}]
;;

let test2 = [ 23, "R"; 27, "R"; 50, "L"; 50, "L"; 100, "R"; 70, "R"; 30, "R" ]

let%expect_test "test2" =
  printer (Testbenches.Day1.run_advanced test2);
  [%expect {| (Result (result.ending_zero_count 4) (result.passing_zero_count 4)) |}]
;;

(* larger numbers... *)
let test3 =
  [ 1050, "R"; 730, "L"; 123430, "R"; 2204, "L"; 1294, "L"; 1010, "R"; 1012, "L" ]
;;

let%expect_test "test3" =
  printer (Testbenches.Day1.run_advanced test3);
  [%expect {| (Result (result.ending_zero_count 3) (result.passing_zero_count 1308)) |}]
;;

let%expect_test "test3 waves" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  printer
    (Testbenches.Day1.run_advanced
       ~trace:`All_named
       ~print_waves_after_test:(fun waves ->
         Waveform.print
           ~display_rules
           ~signals_width:30
           ~display_width:120
           ~wave_width:1
           waves)
       test3);
  [%expect
    {|
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │                            ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───────┬───────────            │
    │day1$cur_dial               ││ 0          │50 │0      │70     │0      │96     │2      │12     │0                      │
    │                            ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───────┴───────────            │
    │                            ││────────────────┬───────────────┬───────────────────────────────┬───────────            │
    │day1$ending_zeros           ││ 0              │1              │2                              │3                      │
    │                            ││────────────────┴───────────────┴───────────────────────────────┴───────────            │
    │day1$i$clear                ││────┐                                                                                   │
    │                            ││    └───────────────────────────────────────────────────────────────────────            │
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │day1$i$data_in_valid        ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐                       │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───────────            │
    │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────            │
    │day1$i$dial_amt             ││ 0          │1050   │730    │123430 │2204   │1294   │1010   │1012                       │
    │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────            │
    │day1$i$finish               ││                                                                    ┌───┐               │
    │                            ││────────────────────────────────────────────────────────────────────┘   └───            │
    │day1$i$is_left              ││                    ┌───────┐       ┌───────────────┐       ┌───────────────            │
    │                            ││────────────────────┘       └───────┘               └───────┘                           │
    │day1$i$start                ││        ┌───┐                                                                           │
    │                            ││────────┘   └───────────────────────────────────────────────────────────────            │
    │                            ││────────────────────────────────────────────────────────────────────────┬───            │
    │day1$o$ending_zero_count    ││ 0                                                                      │3              │
    │                            ││────────────────────────────────────────────────────────────────────────┴───            │
    │                            ││────────────────────────────────────────────────────────────────────────┬───            │
    │day1$o$passing_zero_count   ││ 0                                                                      │13.            │
    │                            ││────────────────────────────────────────────────────────────────────────┴───            │
    │day1$o$valid                ││                                                                        ┌───            │
    │                            ││────────────────────────────────────────────────────────────────────────┘               │
    │                            ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────            │
    │day1$passing_zeros          ││ 0              │11     │18     │1253   │1275   │1287   │1297   │1308                   │
    │                            ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────            │
    └────────────────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘
    (Result (result.ending_zero_count 3) (result.passing_zero_count 1308))
    |}]
;;
