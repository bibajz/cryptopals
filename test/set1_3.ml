open Cryptopals

module To_test = struct
  let xor_single = Set1_3.xor_single

  let xor_single_to_hexstring = Set1_3.xor_single_to_hexstring

  let int_range = Set1_3.int_range

  let decrypt_to_ascii = Set1_3.decrypt_to_ascii
end

let test_xor_single_is_involutive () =
  Alcotest.(check string)
    "" "abcdef"
    (To_test.xor_single_to_hexstring
       (To_test.xor_single_to_hexstring "abcdef" 'c')
       'c')

let test_int_range_empty_same_start_stop =
  QCheck.Test.make ~count:10 ~name:"int_range_invariant" QCheck.pos_int
    (fun i -> To_test.int_range i i = [])

let test_int_range_start_stop_switch_invariant =
  QCheck.Test.make (QCheck.tup2 QCheck.small_int QCheck.small_int)
    (fun (a, b) ->
      QCheck.assume (a <> b);
      let op = if a < b then ( - ) else ( + ) in
      To_test.int_range a b = List.rev (To_test.int_range (op b 1) (op a 1)))

let test_solution () =
  Alcotest.(check string)
    "" "Cooking MC's like a pound of bacon"
    (To_test.decrypt_to_ascii
       "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
       'X')

let test_cases =
  [ ( "xor_single_to_hexstring - involution"
    , `Quick
    , test_xor_single_is_involutive )
  ; QCheck_alcotest.to_alcotest test_int_range_empty_same_start_stop
  ; QCheck_alcotest.to_alcotest test_int_range_start_stop_switch_invariant
  ; ("set 1.3 - solution", `Quick, test_solution)
  ]
