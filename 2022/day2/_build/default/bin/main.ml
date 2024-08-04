type rps = Rock | Paper | Sciccor
type rps_outcome = Lose | Draw | Win

let rps_outcome_score = function Lose -> 0 | Draw -> 3 | Win -> 6
let rps_choice_score = function Rock -> 1 | Paper -> 2 | Sciccor -> 3

let rps_round_outcome opponent_choice my_choice =
  match (my_choice, opponent_choice) with
  | Rock, Sciccor -> Win
  | Paper, Rock -> Win
  | Sciccor, Paper -> Win
  | Rock, Rock | Paper, Paper | Sciccor, Sciccor -> Draw
  | _ -> Lose

let rps_round_score opponent_choice my_choice =
  rps_outcome_score (rps_round_outcome opponent_choice my_choice)
  + rps_choice_score my_choice

let parse_opponent_choice = function
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Sciccor
  | _ -> assert false

let parse_my_choice = function
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Sciccor
  | _ -> assert false

let parse_desired_outcome = function
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> assert false

let rps_desired_outcome_choice opponent_choice desired_outcome =
  match (opponent_choice, desired_outcome) with
  | Rock, Win -> Paper
  | Paper, Win -> Sciccor
  | Sciccor, Win -> Rock
  | Rock, Lose -> Sciccor
  | Paper, Lose -> Rock
  | Sciccor, Lose -> Paper
  | opponent_choice, Draw -> opponent_choice

let rps_round_score_from_desired_outcome opponent_choice desired_outcome =
  let my_choice = rps_desired_outcome_choice opponent_choice desired_outcome in
  rps_outcome_score desired_outcome + rps_choice_score my_choice

let part1 () =
  let ic = open_in Sys.argv.(1) in
  let rec total_score channel accum_score =
    try
      let opponent_choice = parse_opponent_choice (input_char channel) in
      let _whitespace = input_char channel in
      let my_choice = parse_my_choice (input_char channel) in
      let _newline = input_char channel in

      total_score channel
        (rps_round_score opponent_choice my_choice + accum_score)
    with End_of_file -> accum_score
  in
  Printf.printf "Total score: %d\n" (total_score ic 0);
  close_in ic

let part2 () =
  let ic = open_in Sys.argv.(1) in
  let rec total_score channel accum_score =
    try
      let opponent_choice = parse_opponent_choice (input_char channel) in
      let _whitespace = input_char channel in
      let desired_outcome = parse_desired_outcome (input_char channel) in
      let _newline = input_char channel in

      total_score channel
        (rps_round_score_from_desired_outcome opponent_choice desired_outcome
        + accum_score)
    with End_of_file -> accum_score
  in
  Printf.printf "Total score: %d\n" (total_score ic 0);
  close_in ic

let () =
  Printf.printf "-- part 1 --\n";
  part1 ();
  Printf.printf "\n-- part 2 --\n";
  part2 ()
