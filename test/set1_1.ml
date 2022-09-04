open Cryptopals

module To_test = struct
  let string_reverse = Set1_1.string_reverse

  let base64_of_hexstring = Set1_1.base64_of_hexstring
end

let test_reverse () = Alcotest.(check string) "" "" (To_test.string_reverse "")

let test_reverse2 () =
  Alcotest.(check string)
    "" "abcdef"
    (To_test.string_reverse (To_test.string_reverse "abcdef"))

let test_base64 () =
  Alcotest.(check string) "" "" (To_test.base64_of_hexstring "")

let test_solution () =
  Alcotest.(check string)
    "" "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    (To_test.base64_of_hexstring
       "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

let test_cases =
  [ ("reverse - empty string", `Quick, test_reverse)
  ; ("reverse - involution", `Quick, test_reverse2)
  ; ("base64 - empty", `Quick, test_base64)
  ; ("set 1.1 - solution", `Quick, test_solution)
  ]
