open Set1_1

exception Uneven_list_length of string

let rec zip l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2
  | [], _ :: _ -> raise (Uneven_list_length "Second list is not exhausted!")
  | _ :: _, [] -> raise (Uneven_list_length "First list is not exhausted!")
  | [], [] -> []

exception Int_too_large

let hexchar_of_int i =
  match i with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'a'
  | 11 -> 'b'
  | 12 -> 'c'
  | 13 -> 'd'
  | 14 -> 'e'
  | 15 -> 'f'
  | _ -> raise Int_too_large

let hexstring_of_int i =
  let lower_powers =
    seq_reverse
      (Seq.take_while (fun x -> x <= i) (Seq.map (pow 16) (Seq.ints 0)))
  in
  let rec hexstring_of_int_inner seq acc num =
    match Seq.uncons seq with
    | None -> acc
    | Some (h, t) ->
      let times = num / h in
      hexstring_of_int_inner t (Seq.cons times acc) (num - (times * h))
  in
  String.of_seq
    (Seq.map hexchar_of_int
       (seq_reverse (hexstring_of_int_inner lower_powers Seq.empty i)))

let xor_buffer b1 b2 =
  String.concat ""
    (List.map hexstring_of_int
       (List.map
          (fun (x, y) -> Int.logxor (int_of_hexstring x) (int_of_hexstring y))
          (zip (split b1 2) (split b2 2))))
