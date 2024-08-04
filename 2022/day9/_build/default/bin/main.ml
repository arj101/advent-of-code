type pos_map = (int * int, unit) Hashtbl.t
type dir = Up | Down | Left | Right | DiagUR | DiagUL | DiagDL | DiagDR

let dir_of_char = function
  | 'L' -> Left
  | 'R' -> Right
  | 'U' -> Up
  | 'D' -> Down
  | _ -> assert false

type pos = { x : int; y : int }
type sim_state = { head : pos; tail : pos }
type move_cmd = { dir : dir; dist : int }

let default_sim_state () = { head = { x = 0; y = 0 }; tail = { x = 0; y = 0 } }

(*does not account for force magnitude (could be zero)*)
let tail_force_dir state =
  let dy = state.head.y - state.tail.y and dx = state.head.x - state.tail.x in
  if abs dy >= 1 && abs dx >= 1 then
    match (dy > 0, dx > 0) with
    | true, true -> DiagUR
    | true, false -> DiagUL
    | false, true -> DiagDR
    | false, false -> DiagDL
  else if dx != 0 then if dx > 0 then Right else Left
  else if dy > 0 then Up
  else Down

let set_tail_pos dx dy state =
  { head = state.head; tail = { x = state.tail.x + dx; y = state.tail.y + dy } }

let rec update_tail state =
  Printf.printf "head at %d %d, tail at %d %d\n" state.head.x state.head.y
    state.tail.x state.tail.y;
  let force_dir = tail_force_dir state in
  let non_zero_force =
    if
      state.head.x - state.tail.x |> abs > 1
      || state.head.y - state.tail.y |> abs > 1
    then true
    else false
  in
  if not non_zero_force then state
  else
    let new_state =
      set_tail_pos
        (match force_dir with
        | Right | DiagUR | DiagDR -> 1
        | Left | DiagDL | DiagUL -> -1
        | _ -> 0)
        (match force_dir with
        | Up | DiagUR | DiagUL -> 1
        | Down | DiagDR | DiagDL -> -1
        | _ -> 0)
        state
    in
    update_tail new_state (* call update_tail till tail is near the head *)

let set_head_pos dx dy state =
  { tail = state.tail; head = { x = dx + state.head.x; y = dy + state.head.y } }

let interpret_input state channel =
  let visit_map = Hashtbl.create 1024 in
  Hashtbl.replace visit_map (0, 0) ();
  let rec aux state =
    try
      Hashtbl.replace visit_map (state.tail.x, state.tail.y) ();
      let dir = input_char channel |> dir_of_char in
      let _whitespace = input_char channel in
      let dir_mag = input_line channel |> int_of_string in
      let rec auxaux state n =
        if n < dir_mag - 1 then
          let new_state =
            state
            |> set_head_pos
                 (match dir with Right -> dir_mag | Left -> -dir_mag | _ -> 0)
                 (match dir with Up -> dir_mag | Down -> -dir_mag | _ -> 0)
            |> update_tail
          in
          auxaux new_state (n + 1)
        else state
      in
      let state = auxaux state 0 in

      Hashtbl.replace visit_map (state.tail.x, state.tail.y) ();
      aux state
    with End_of_file -> (state, visit_map)
  in
  aux state

exception UnhandledCondition

let signum n = if n == 0 then 0 else if n > 0 then 1 else -1

type vec2 = { x : int; y : int }

let diff state =
  { x = state.head.x - state.tail.x; y = state.head.y - state.tail.y }

let interpret_input' channel =
  let visit_map = Hashtbl.create 1024 in
  Hashtbl.replace visit_map (0, 0) ();

  let rec aux state =
    try
      let dir = input_char channel in
      let _ = input_char channel in
      let mag = input_line channel |> int_of_string in

      let rec sim_loop state n =
        if n >= mag then state
        else
          let state =
            match dir with
            | 'R' -> set_head_pos 1 0 state
            | 'L' -> set_head_pos (-1) 0 state
            | 'U' -> set_head_pos 0 (-1) state
            | 'D' -> set_head_pos 0 1 state
            | _ -> raise UnhandledCondition
          in

          let { x = dx; y = dy } = diff state in
          let state =
            if abs dx > 1 || abs dy > 1 then
              set_tail_pos (signum dx) (signum dy) state
            else state
          in
          Hashtbl.replace visit_map (state.tail.x, state.tail.y) ();

          sim_loop state (n + 1)
      in
      sim_loop state 0 |> aux
    with End_of_file -> (state, visit_map)
  in

  aux (default_sim_state ())

type rope = Con of vec2 * rope | Tail of vec2

let move_rope_head dx dy rope =
  match rope with
  | Con (pos, next) -> Con ({ x = pos.x + dx; y = pos.y + dy }, next)
  | Tail pos -> Tail { x = pos.x + dx; y = pos.y + dy }

let dx_dy v1 v2 = { x = v1.x - v2.x; y = v1.y - v2.y }
let v_add v dx dy = { x = v.x + dx; y = v.y + dy }

let step_rope_sim (rope : rope) =
  let rec aux (parent_pos : vec2) (remaining_rope : rope) =
    let new_pos pos =
      let diff = dx_dy parent_pos pos in
      if diff.x |> abs > 1 || diff.y |> abs > 1 then
        v_add pos (signum diff.x) (signum diff.y)
      else pos
    in
    match remaining_rope with
    | Con (pos, next) ->
        let new_pos = new_pos pos in
        Con (new_pos, aux new_pos next)
    | Tail pos -> Tail (new_pos pos)
  in
  match rope with
  | Con (pos, next) -> Con (pos, aux pos next)
  | Tail pos -> Tail pos

let rec rope_tail_pos = function 
  | Con (_, next) -> rope_tail_pos next
  | Tail pos -> pos

let create_rope length =
  let rec aux (rope : rope) (n : int) =
    if n < length then aux (Con ({ x = 0; y = 0 }, rope)) (n + 1) else rope
  in
  aux (Tail { x = 0; y = 0 }) 0

let part2 channel = 
  let rec visit_map = Hashtbl.create 2048 in
  let rec aux rope =
    try
    let dir = input_char channel in
    let _ = input_char channel in
    let mag = input_line channel |> int_of_string in 

    let rec sim_loop rope n =
      if n >= mag then rope 
      else 
        let rope = match dir with 
        | 'D' -> move_rope_head 0 1 rope 
        | 'U' -> move_rope_head 0 (-1) rope 
        | 'L' -> move_rope_head (-1) 0 rope 
        | 'R' -> move_rope_head 1 0 rope
        | _ -> raise UnhandledCondition
        in
        let rope = step_rope_sim rope in
        let tail_pos = rope_tail_pos rope in
        Hashtbl.replace visit_map (tail_pos.x, tail_pos.y) ();
        Printf.printf "( %d %d )" tail_pos.x tail_pos.y;
        sim_loop rope (n+1)
    in
    let rope = sim_loop rope 0 in
    aux rope

    with End_of_file -> visit_map
  in
  aux (create_rope 10)

let () =
  let ic = open_in "./input.txt" in
  let state, visit_map = interpret_input' ic in
  Printf.printf "Tail visited %d positions\n" (Hashtbl.length visit_map);
  close_in ic;
  let ic = open_in "./input.txt" in
  let visit_map = part2 ic in 
  Printf.printf "Tail visited %d positions (long rope)\n" (Hashtbl.length visit_map);
(* let final_state, visit_map = interpret_input (default_sim_state ()) ic in *)
(* Printf.printf "Tail visited %d positions\n" (Hashtbl.length visit_map) *)
