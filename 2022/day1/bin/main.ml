let part1 () =
  let ic = open_in "./input.txt" in

  let rec max_calories channel curr_max =
    try
      let line = input_line channel in
      let calorie0 = int_of_string line in
      let rec elf_calorie channel accum =
        try
          let line = input_line channel in
          match int_of_string_opt line with
          | Some c -> elf_calorie channel (accum + c)
          | None -> accum
        with End_of_file -> accum
      in
      let max_calorie = max curr_max (elf_calorie channel calorie0) in
      max_calories channel max_calorie
    with End_of_file -> curr_max
  in

  Printf.printf "highest calorie: %d\n" (max_calories ic 0);
  close_in ic

let push top_three value =
  let fst, scnd, third = top_three in
  if value < third then  top_three else
  if value < scnd then (fst, scnd, value) else 
  if value < fst then (fst, value, scnd) else
  (value, fst, scnd)

let sum (fst, scnd, third) = fst + scnd + third
let fst (f, _, _) = f
let scnd (_, s, _) = s
let third (_, _, t) = t

let part2 () =
  let ic = open_in "./input.txt" in
  let rec find_top_three channel top_three_list =
    try
      let line = input_line channel in
      let calorie0 = int_of_string line in
      let rec elf_calorie channel accum =
        try
          let line = input_line channel in
          match int_of_string_opt line with
          | Some c -> elf_calorie channel (accum + c)
          | None -> accum
        with End_of_file -> accum
      in
      let top_three = push top_three_list (elf_calorie channel calorie0) in
      find_top_three channel top_three
    with End_of_file -> top_three_list
  in

  let top_three = find_top_three ic (0, 0, 0) in

  Printf.printf "1: %d\n2: %d\n3: %d\n" (fst top_three) (scnd top_three)
    (third top_three);
  Printf.printf "Sum of top three: %d\n" (sum top_three);
  close_in ic

let () =
  Printf.printf "-- part 1 --\n";
  part1 ();
  Printf.printf "\n-- part 2 --\n";
  part2 ()
