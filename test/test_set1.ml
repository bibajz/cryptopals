open OUnit2
open Cryptopals.Set1_1

let tests =
  "test suite set1_1"
  >::: [ ( "string_reverse_empty" >:: fun _ ->
           assert_equal "" (string_reverse "") )
       ; ( "string_reverse_idempotence" >:: fun _ ->
           assert_equal "abcdef" (string_reverse (string_reverse "abcdef")) )
       ; ( "string_reverse_reverse-commutativity" >:: fun _ ->
           assert_equal
             (string_reverse ("abc" ^ "def"))
             (string_reverse "def" ^ string_reverse "abc") )
       ; ( "hexstring_to_base64_empty" >:: fun _ ->
           assert_equal "" (hexstring_to_base64 "") )
       ; ( "Set 1.1 solution" >:: fun _ ->
           assert_equal
             "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
             (hexstring_to_base64
                "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
         )
       ]

let _ = run_test_tt_main tests
