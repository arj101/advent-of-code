exception UnexpectedChar

let consume channel c =
  if input_char channel == c then () else raise UnexpectedChar

let consume_opt channel c = if input_char channel == c then Some () else None

let parse_crate_opt channel =
  let map = Option.map in

  consume_opt channel '['
  |> map (fun () -> input_char channel)
  |> map (fun crate -> consume_opt channel ']' |> map (fun () -> crate))
  |> Option.join

let parse_crate channel =
  consume channel '[';
  let crate = input_char channel in
  consume channel ']';
  crate

let parse_number channel =
  let rec parse_aux channel =
    match input_char channel with
    | ' ' -> parse_aux channel
    | '\n' -> None
    | '0' .. '9' as c ->
        let start_pos = In_channel.pos channel in
        let rec parse_aux_aux channel buf =
          match input_char channel with
          | '0' .. '9' as c ->
              Buffer.add_char buf c;
              parse_aux_aux channel buf
          | _ -> int_of_string (Buffer.contents buf)
        in
        let buf = Buffer.create 2 in
        Buffer.add_char buf c;
        let num = parse_aux_aux channel buf in
        Some (num, Int64.to_int start_pos - 1)
    | c ->
        Printf.printf " '%c' " c;
        assert false
  in
  parse_aux channel

let rec consume_whitespace q =
  match Queue.peek q with
  | ' ' ->
      Queue.pop q |> ignore;
      consume_whitespace q
  | _ -> ()

type crate_stacks = char Stack.t array
type mov_cmd = { n : int; source : int; dest : int }

let parse_crate_index_line channel =
  let rec first_non_space_char channel =
    match input_char channel with ' ' -> first_non_space_char channel | c -> c
  in
  let rec drop_to_index_line channel =
    let prev_pos = In_channel.pos channel in
    let c = first_non_space_char channel in
    match c with
    | '[' ->
        input_line channel |> ignore;
        drop_to_index_line channel
    | _ -> In_channel.seek channel prev_pos
  in
  drop_to_index_line channel;
  (*
  Printf.printf " line -> %s" (input_line channel);
*)
  let index_line_start = In_channel.pos channel |> Int64.to_int in

  let rec collect_indices channel accum =
    match parse_number channel with
    | None -> List.rev accum
    | Some (num, pos) ->
        collect_indices channel ((num, pos - index_line_start) :: accum)
  in
  let indices = collect_indices channel [] in
  In_channel.seek channel (Int64.of_int 0);
  (index_line_start, indices)

let parse_stack_repr channel : crate_stacks =
  let index_line_start, indices = parse_crate_index_line channel in
  let line_length = (4 * List.length indices) - 1 in
  let num_lines = index_line_start / (line_length + 1) in
  (*+1 to account for newline character*)
  let pos_of_offset_at_line offset line = ((line_length + 1) * line) + offset in

  let crate_stacks =
    Array.init (List.length indices) (fun _ -> Stack.create ())
  in

  List.iter
    (fun (num, pos) ->
      let rec print_all_lines curr_line =
        if curr_line < 0 then ()
        else
          let pos = pos_of_offset_at_line pos curr_line in
          In_channel.seek channel (Int64.of_int pos);
          let crate_char = input_char channel in
          if crate_char != ' ' then (
            Printf.printf " [%c]" crate_char;
            crate_stacks.(num - 1) |> Stack.push crate_char)
          else ();
          print_all_lines (curr_line - 1)
      in
      Printf.printf "%d =>" num;
      print_all_lines (num_lines - 1);
      print_newline ();
      ())
    indices;
  In_channel.seek channel (Int64.of_int index_line_start);
  input_line channel |> ignore;
  input_line channel |> ignore;

  crate_stacks

let parse_command channel =
  let line = input_line channel in
  let words = String.split_on_char ' ' line |> Array.of_list in
  (*[| "move" n "from" source "to" dest *)
  let n = int_of_string words.(1) in
  let source = int_of_string words.(3) in
  let dest = int_of_string words.(5) in
  { n; source; dest }

let exec_command_cratemover9000 crate_stacks { n; source; dest } =
  let rec move_crates remaining =
    if remaining <= 0 then ()
    else
      let crate = crate_stacks.(source - 1) |> Stack.pop in
      crate_stacks.(dest - 1) |> Stack.push crate;
      move_crates (remaining - 1)
  in
  move_crates n

let exec_command_cratemover9001 crate_stacks { n; source; dest } =
  let picked_up_crates = Stack.create () in
  let rec pickup_crates remaining =
    if remaining <= 0 then ()
    else
      let crate = crate_stacks.(source - 1) |> Stack.pop in
      Stack.push crate picked_up_crates;
      pickup_crates (remaining - 1)
  in
  let rec drop_crates remaining =
    if remaining <= 0 then ()
    else
      let crate = Stack.pop picked_up_crates in
      crate_stacks.(dest - 1) |> Stack.push crate;
      drop_crates (remaining - 1)
  in
  pickup_crates n;
  drop_crates n

let rec parse_and_exec_commands_cratemover9000 channel crate_stacks =
  try
    let mov_cmd = parse_command channel in
    exec_command_cratemover9000 crate_stacks mov_cmd;
    parse_and_exec_commands_cratemover9000 channel crate_stacks
  with End_of_file -> ()

let rec parse_and_exec_commands_cratemover9001 channel crate_stacks =
  try
    let mov_cmd = parse_command channel in
    exec_command_cratemover9001 crate_stacks mov_cmd;
    parse_and_exec_commands_cratemover9001 channel crate_stacks
  with End_of_file -> ()

let part1 () =
  let ic = open_in "./input.txt" in
  let crate_stacks = parse_stack_repr ic in
  parse_and_exec_commands_cratemover9000 ic crate_stacks;

  Printf.printf "Crates at the top: ";
  Array.iter (fun s -> Printf.printf "%c" (Stack.top s)) crate_stacks;
  print_newline ()

let part2 () = 
  let ic = open_in "./input.txt" in
  let crate_stacks = parse_stack_repr ic in
  parse_and_exec_commands_cratemover9001 ic crate_stacks;

  Printf.printf "Crates at the top: ";
  Array.iter (fun s -> Printf.printf "%c" (Stack.top s)) crate_stacks;
  print_newline ()

let () =
  print_string "-- part 1 --\n";
  part1 ();
  print_string "\n-- part 2 --\n";
  part2 ()
