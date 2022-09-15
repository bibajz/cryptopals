open Set1_1

let xor_single (b : string) (c : char) : int list =
  List.map (fun h -> Int.logxor (int_of_hexstring h) (Char.code c)) (split b 2)

let xor_single_to_hexstring (b : string) (c : char) : string =
  (* Including a module for only 1 function ... nifty *)
  let open Set1_2 in
  xor_single b c |> List.map hexstring_of_int |> String.concat ""

let int_range a b =
  let fn = if a < b then Int.succ else Int.pred in
  let rec int_range_inner c d acc =
    match c = d with
    | false -> int_range_inner (fn c) d (Seq.cons c acc)
    | true -> acc
  in
  List.of_seq (seq_reverse (int_range_inner a b Seq.empty))

let digits = List.map Char.chr (int_range 48 58)

let uppercase_alphabet = List.map Char.chr (int_range 65 91)

let lowercase_alphabet = List.map Char.lowercase_ascii uppercase_alphabet

let decrypt_to_ascii (b : string) (c : char) : string =
  xor_single b c |> List.map Char.chr |> List.to_seq |> String.of_seq
