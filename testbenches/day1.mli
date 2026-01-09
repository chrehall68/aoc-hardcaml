open! Core
open! Hardcaml
open! Hardcaml_test_harness

type result =
  { ending_zero_count : int
  ; passing_zero_count : int
  }

val run_advanced
  :  ?waves_config:Waves_config.t
  -> ?trace:[ `All_named | `Everything | `Ports_only ]
  -> ?print_waves_after_test:(Hardcaml_waveterm.Waveform.t -> unit)
  -> (int * string) list
  -> result
