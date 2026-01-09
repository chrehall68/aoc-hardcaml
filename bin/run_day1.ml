open! Core
open! Stdio.In_channel
open! Testbenches.Day1
open! Hardcaml

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
  let result = run_advanced input_tuples in
  print_s
    [%message "Result" (result.ending_zero_count : int) (result.passing_zero_count : int)]
;;
