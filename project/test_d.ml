open OUnit2

let tests = [
  "this is a dummy test" >:: (fun _ -> 
    assert_equal true true);
]
