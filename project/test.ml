open OUnit2

open Test_d
open Test_k
open Test_v

let tests_merged = 
  List.flatten [
    Test_d.tests;
    Test_k.tests;
    Test_v.tests
  ]

let suite = 
  "test suite for project" >::: tests_merged

let _ = run_test_tt_main suite
