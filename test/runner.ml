let () =
  let open Alcotest in
  run "Test runner"
    [ ("set 1.1", Set1_1.test_cases)
    ; ("set 1.2", Set1_2.test_cases)
    ; ("set 1.3", Set1_3.test_cases)
    ]
