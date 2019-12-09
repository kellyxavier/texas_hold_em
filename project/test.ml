(** We split out tests amongst three separate files that are all merged into one
    suite in test.ml so that we could each add tests as needed without the need
    to worry about merge conflicts.

    Our general unit testing strategy was glass box, we would try out functions
    that the game would use based on the possible values we knew it would need
    to take in (making sure to use edge cases whenever they existed)
    to make sure that it would handle the input it would be given by
    the game as we expected it to. This worked for the modules with simple
    functions (state, rank_hand, etc.) but when it came to the modules with 
    functions that took terminal input (main) we resorted to playtesting 
    in order to find any anomalies and bugs. 

    In the event that a bug was found in a simple function during playtesting,
    a test case was created to reflect that bug and added to our suite, 
    whereupon we could then make sure that it was fixed in future changes to our
    project. *)


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
