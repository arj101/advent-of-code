let item_priority = function
  | 'a' .. 'z' as c -> 1 + (Char.code c - Char.code 'a')
  | 'A' .. 'Z' as c -> 27 + (Char.code c - Char.code 'A')
  | _ -> assert false

let find_common_item rucksack =
  let rucksack_halflength = String.length rucksack / 2 in
  let comp1, comp2 =
    ( String.sub rucksack 0 rucksack_halflength,
      String.sub rucksack rucksack_halflength rucksack_halflength )
  in
  String.to_seq comp1
  |> Seq.filter (fun c_comp1 -> String.contains comp2 c_comp1)

let common_item_priority_sum rucksack =
  let common_item = find_common_item rucksack in
  let _, sum =
    Seq.fold_left
      (fun (already_found, acc) item ->
        if
          List.find_opt (fun found_item -> found_item == item) already_found
          |> Option.is_some
        then (already_found, acc)
        else (item :: already_found, acc + item_priority item))
      ([], 0) common_item
  in
  sum

let group_common_item r1 r2 r3 =
  let r1_seq = String.to_seq r1 in
  Seq.filter (fun i1 -> String.contains r2 i1 && String.contains r3 i1) r1_seq |> List.of_seq

let group_common_item_priority_sum r1 r2 r3 =
  match group_common_item r1 r2 r3 with
  | common_item::_ -> item_priority common_item
  | [] -> 0

let part1 () =
  let ic = open_in "./input.txt" in
  let rec find_priority_sum channel accum_sum =
    try
      let line = input_line channel in
      find_priority_sum channel (accum_sum + common_item_priority_sum line)
    with End_of_file -> accum_sum
  in
  let priority_sum = find_priority_sum ic 0 in
  Printf.printf "Priority sum: %d\n" priority_sum;
  close_in ic

let part2 () = 
  let ic = open_in "./input.txt" in
  let rec find_priority_sum channel accum_sum =
    try
      let line1 = input_line channel in
      let line2 = input_line channel in
      let line3 = input_line channel in
      find_priority_sum channel (accum_sum + group_common_item_priority_sum line1 line2 line3)
    with End_of_file -> accum_sum
  in
  let priority_sum = find_priority_sum ic 0 in
  Printf.printf "Badge priority sum: %d\n" priority_sum;
  close_in ic

let () =
  Printf.printf "-- part 1 --\n";
  part1 ();
  Printf.printf "\n-- part 2 --\n";
  part2 ()
