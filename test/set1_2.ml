open Cryptopals

module To_test = struct
  let hexstring_of_int = Set1_2.hexstring_of_int

  let xor_buffer = Set1_2.xor_buffer
end

let test_hex () = Alcotest.(check string) "" "ff" (To_test.hexstring_of_int 255)

let test_hex2 () = Alcotest.(check string) "" "00" (To_test.hexstring_of_int 0)

let test_solution () =
  Alcotest.(check string)
    "" "746865206b696420646f6e277420706c6179"
    (To_test.xor_buffer "1c0111001f010100061a024b53535009181c"
       "686974207468652062756c6c277320657965")

let test_cases =
  [ ("hex of int - 255", `Quick, test_hex)
  ; ("hex of int - 0", `Quick, test_hex2)
  ; ("set 1.2 - solution", `Quick, test_solution)
  ]
