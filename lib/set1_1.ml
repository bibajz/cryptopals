let string_after s n =
  String.of_seq
    (Seq.filter_map
       (function
         | i, x -> (
           match i < n with
           | true -> None
           | _ -> Some x))
       (String.to_seqi s))

let rec split s n =
  let l = String.length s in
  match l < n with
  | true -> (
    match l = 0 with
    | true -> []
    | _ -> [ s ])
  | false -> [ String.sub s 0 n ] @ split (string_after s n) n

let seq_reverse s =
  let rec rev_inner s_inner acc =
    match Seq.uncons s_inner with
    | None -> acc
    | Some (h, t) -> rev_inner t (Seq.cons h acc)
  in
  rev_inner s Seq.empty

let string_reverse s = String.of_seq (seq_reverse (String.to_seq s))

exception Invalid_hex_char

let int_of_hexchar c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | _ -> raise Invalid_hex_char

let pow a b = Seq.fold_left (fun a b -> a * b) 1 (Seq.take b (Seq.repeat a))

let int_of_hexstring s =
  let rec int_of_hexstring_inner seq acc c =
    match Seq.uncons seq with
    | None -> acc
    | Some (h, t) ->
      int_of_hexstring_inner t (acc + (int_of_hexchar h * pow 16 c)) (c + 1)
  in
  int_of_hexstring_inner (seq_reverse (String.to_seq s)) 0 0

let first_n_bits num n = num mod Int.shift_left 1 n

let n_bits_after_m num n m = first_n_bits (Int.shift_right num m) n

type base64 =
  | Char of int
  | Padding

let rec bytelist_to_base64 l =
  match l with
  | e1 :: e2 :: e3 :: t ->
    [ Char (n_bits_after_m e1 6 2)
    ; Char (Int.shift_left (first_n_bits e1 2) 4 + n_bits_after_m e2 4 4)
    ; Char (Int.shift_left (first_n_bits e2 4) 2 + n_bits_after_m e3 2 6)
    ; Char (first_n_bits e3 6)
    ]
    @ bytelist_to_base64 t
  | [ e1; e2 ] ->
    [ Char (n_bits_after_m e1 6 2)
    ; Char (Int.shift_left (first_n_bits e1 2) 4 + n_bits_after_m e2 4 4)
    ; Char (Int.shift_left (first_n_bits e2 4) 2)
    ; Padding
    ]
  | [ e1 ] ->
    [ Char (n_bits_after_m e1 6 2)
    ; Char (Int.shift_left (first_n_bits e1 2) 4)
    ; Padding
    ; Padding
    ]
  | [] -> []

exception Invalid_sextet

let sextet_to_base64 n =
  match n with
  | 0 -> 'A'
  | 1 -> 'B'
  | 2 -> 'C'
  | 3 -> 'D'
  | 4 -> 'E'
  | 5 -> 'F'
  | 6 -> 'G'
  | 7 -> 'H'
  | 8 -> 'I'
  | 9 -> 'J'
  | 10 -> 'K'
  | 11 -> 'L'
  | 12 -> 'M'
  | 13 -> 'N'
  | 14 -> 'O'
  | 15 -> 'P'
  | 16 -> 'Q'
  | 17 -> 'R'
  | 18 -> 'S'
  | 19 -> 'T'
  | 20 -> 'U'
  | 21 -> 'V'
  | 22 -> 'W'
  | 23 -> 'X'
  | 24 -> 'Y'
  | 25 -> 'Z'
  | 26 -> 'a'
  | 27 -> 'b'
  | 28 -> 'c'
  | 29 -> 'd'
  | 30 -> 'e'
  | 31 -> 'f'
  | 32 -> 'g'
  | 33 -> 'h'
  | 34 -> 'i'
  | 35 -> 'j'
  | 36 -> 'k'
  | 37 -> 'l'
  | 38 -> 'm'
  | 39 -> 'n'
  | 40 -> 'o'
  | 41 -> 'p'
  | 42 -> 'q'
  | 43 -> 'r'
  | 44 -> 's'
  | 45 -> 't'
  | 46 -> 'u'
  | 47 -> 'v'
  | 48 -> 'w'
  | 49 -> 'x'
  | 50 -> 'y'
  | 51 -> 'z'
  | 52 -> '0'
  | 53 -> '1'
  | 54 -> '2'
  | 55 -> '3'
  | 56 -> '4'
  | 57 -> '5'
  | 58 -> '6'
  | 59 -> '7'
  | 60 -> '8'
  | 61 -> '9'
  | 62 -> '+'
  | 63 -> '/'
  | _ -> raise Invalid_sextet

let to_base64 c =
  match c with
  | Char n -> sextet_to_base64 n
  | Padding -> '='

let bytelist_to_base64_list l = List.map to_base64 (bytelist_to_base64 l)

(* TODO: Convert to Seq? *)
let base64_of_hexstring s =
  String.of_seq
    (List.to_seq
       (bytelist_to_base64_list (List.map int_of_hexstring (split s 2))))
