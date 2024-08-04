type range = { fst : int; lst : int }

let range_fully_contains_other r1 r2 =
  let r1_contains_r2 r1 r2 = r1.fst <= r2.fst && r1.lst >= r2.lst in
  r1_contains_r2 r1 r2 || r1_contains_r2 r2 r1

let range_has_overlap r1 r2 =
  let has_overlap_aux r1 r2 =
    (r2.fst >= r1.fst && r2.fst <= r1.lst)
    || (r2.lst >= r1.fst && r2.lst <= r1.lst)
  in
  has_overlap_aux r1 r2 || has_overlap_aux r2 r1

let parse_range s =
  match String.split_on_char '-' s with
  | [ n1; n2 ] -> { fst = int_of_string n1; lst = int_of_string n2 }
  | _ -> assert false

let parse_range_pair s =
  match String.split_on_char ',' s with
  | [ r1; r2 ] -> (parse_range r1, parse_range r2)
  | _ -> assert false

let part1 () =
  let ic = open_in "./input.txt" in
  let rec contained_range_count channel accum_count =
    try
      let line = input_line channel in
      let r1, r2 = parse_range_pair line in
      let accum_count =
        if range_fully_contains_other r1 r2 then 1 + accum_count
        else accum_count
      in
      contained_range_count channel accum_count
    with End_of_file -> accum_count
  in
  let count = contained_range_count ic 0 in
  Printf.printf "%d pairs fully contain the other\n" count;
  close_in ic

let part2 () =
  let ic = open_in "./input.txt" in
  let rec contained_range_count channel accum_count =
    try
      let line = input_line channel in
      let r1, r2 = parse_range_pair line in
      let accum_count =
        if range_has_overlap r1 r2 then 1 + accum_count else accum_count
      in
      contained_range_count channel accum_count
    with End_of_file -> accum_count
  in
  let count = contained_range_count ic 0 in
  Printf.printf "%d pairs have overlap\n" count;
  close_in ic

let () =
  Printf.printf "-- part 1 --\n";
  part1 ();
  Printf.printf "\n-- part 2 --\n";
  part2 ()
