open Bigarray

module BoxChar = struct
  type t = LTCorner | RTCorner | LBCorner | RBCorner | None

  let make t =
    match t with
    | RTCorner -> "┐"
    | LTCorner -> "┌"
    | LBCorner -> "└"
    | RBCorner -> "┘"
    | None -> " "
end

module Vis = struct
  type t = int
  (*
  layout:
    0b{LEFT}{BOTTOM}{TOP}{RIGHT}
    where {x} is a single bit
  *)

  let new_invisible () : t = 0
  let left_visible vis = vis lor 0b1000
  let right_visible vis = vis lor 0b0001
  let top_visible vis = vis lor 0b0010
  let bottom_visible vis = vis lor 0b0100
  let left_is_visible vis = (vis lsr 3) land 0b1 == 1
  let right_is_visible vis = vis land 0b1 == 1
  let top_is_visible vis = (vis lsr 1) land 0b1 == 1
  let bottom_is_visible vis = (vis lsr 2) land 0b1 == 1
  let is_visible vis = vis land 0b1111 > 0
end

let dimensions channel =
  let w = input_line channel |> String.length in
  let rec find_height accum =
    try
      input_line channel |> ignore;
      find_height (accum + 1)
    with End_of_file -> accum
  in

  let dim = (w, find_height 1) in
  In_channel.seek channel (Int64.of_int 0);
  dim

let trees_dimension trees = (Array2.dim1 trees, Array2.dim2 trees)

let array2_for_each f trees =
  let w, h = trees_dimension trees in
  let rec for_row y =
    let rec for_tree x =
      f x y (Array2.get trees x y);
      if x < w - 1 then for_tree (x + 1) else ()
    in
    for_tree 0;
    if y < h - 1 then for_row (y + 1) else ()
  in
  for_row 0

let make_vis_map trees =
  let w, h = trees_dimension trees in
  let vis_map = Array2.create Int C_layout w h in
  array2_for_each
    (fun x y _ -> Array2.set vis_map x y (Vis.new_invisible ()))
    trees;
  vis_map

let print_vis_map map =
  let w, h = trees_dimension map in
  let rec print_row y =
    let rec print_tree x sub_row =
      let vis = Array2.get map x y in
      (if sub_row == 0 then (
         let char1 =
           if Vis.left_is_visible vis || Vis.top_is_visible vis then
             BoxChar.make LTCorner
           else BoxChar.make None
         in
         let char2 =
           if Vis.top_is_visible vis || Vis.right_is_visible vis then
             BoxChar.make RTCorner
           else BoxChar.make None
         in
         print_string char1;
         print_string char2)
       else
         let char1 =
           if Vis.left_is_visible vis || Vis.bottom_is_visible vis then
             BoxChar.make LBCorner
           else BoxChar.make None
         in
         let char2 =
           if Vis.bottom_is_visible vis || Vis.right_is_visible vis then
             BoxChar.make RBCorner
           else BoxChar.make None
         in
         print_string char1;
         print_string char2);
      if x < w - 1 then print_tree (x + 1) sub_row else ()
    in
    print_tree 0 0;
    print_newline ();
    print_tree 0 1;
    print_newline ();
    if y < h - 1 then print_row (y + 1) else ()
  in
  print_row 0

let parse channel =
  let w, h = dimensions channel in
  let trees = Array2.create Int C_layout w h in
  let rec parse_lines y =
    let rec parse_char x =
      match input_char channel with
      | '\n' -> ()
      | c ->
          let c = String.make 1 c |> int_of_string in
          Array2.set trees x y c;
          parse_char (x + 1)
    in
    try
      parse_char 0;
      parse_lines (y + 1)
    with End_of_file -> ()
  in
  parse_lines 0;
  trees

let row_look_from_left row trees vis_map =
  let w, _ = trees_dimension trees in

  let set_vis x =
    let vis = Array2.get vis_map x row in
    Array2.set vis_map x row (Vis.left_visible vis)
  in
  let tree_height x = Array2.get trees x row in

  let rec row_iter loc_accum curr_tallest x =
    let curr_height = tree_height x in
    let curr_tallest, loc_accum =
      if curr_height > curr_tallest then (
        set_vis x;
        (curr_height, (x, row) :: loc_accum))
      else (curr_tallest, loc_accum)
    in
    if x < w - 1 then row_iter loc_accum curr_tallest (x + 1) else loc_accum
  in
  row_iter [] (-1) 0

let row_look_from_right row trees vis_map =
  let w, _ = trees_dimension trees in

  let set_vis x =
    let vis = Array2.get vis_map x row in
    Array2.set vis_map x row (Vis.right_visible vis)
  in
  let tree_height x = Array2.get trees x row in

  let rec row_iter loc_accum curr_tallest x =
    let curr_height = tree_height x in
    let curr_tallest, loc_accum =
      if curr_height > curr_tallest then (
        set_vis x;
        (curr_height, (x, row) :: loc_accum))
      else (curr_tallest, loc_accum)
    in
    if x > 0 then row_iter loc_accum curr_tallest (x - 1) else loc_accum
  in
  row_iter [] (-1) (w - 1)

let col_look_from_top col trees vis_map =
  let _, h = trees_dimension trees in

  let set_vis y =
    let vis = Array2.get vis_map col y in
    Array2.set vis_map col y (Vis.top_visible vis)
  in
  let tree_height y = Array2.get trees col y in

  let rec col_iter loc_accum curr_tallest y =
    let curr_height = tree_height y in
    let curr_tallest, loc_accum =
      if curr_height > curr_tallest then (
        set_vis y;
        (curr_height, (col, y) :: loc_accum))
      else (curr_tallest, loc_accum)
    in
    if y < h - 1 then col_iter loc_accum curr_tallest (y + 1) else loc_accum
  in
  col_iter [] (-1) 0

let col_look_from_bottom col trees vis_map =
  let _, h = trees_dimension trees in

  let set_vis y =
    let vis = Array2.get vis_map col y in
    Array2.set vis_map col y (Vis.bottom_visible vis)
  in
  let tree_height y = Array2.get trees col y in

  let rec col_iter loc_accum curr_tallest y =
    let curr_height = tree_height y in
    let curr_tallest, loc_accum =
      if curr_height > curr_tallest then (
        set_vis y;
        (curr_height, (col, y) :: loc_accum))
      else (curr_tallest, loc_accum)
    in
    if y > 0 then col_iter loc_accum curr_tallest (y - 1) else loc_accum
  in
  col_iter [] (-1) (h - 1)

let look_from_left_right trees vis_map =
  let _, h = trees_dimension trees in
  let rec aux loc_accum y =
    let loc_accum =
      loc_accum
      @ row_look_from_left y trees vis_map
      @ row_look_from_right y trees vis_map
    in
    if y < h - 1 then aux loc_accum (y + 1) else ()
  in
  aux [] 0

let look_from_top_bottom trees vis_map =
  let w, _ = trees_dimension trees in
  let rec aux loc_accum x =
    let loc_accum =
      loc_accum
      @ col_look_from_top x trees vis_map
      @ col_look_from_bottom x trees vis_map
    in
    if x < w - 1 then aux loc_accum (x + 1) else loc_accum
  in
  aux [] 0

let visible_count vis_map =
  let count = ref 0 in
  array2_for_each
    (fun _ _ visible -> if visible > 0 then count := !count + 1 else ())
    vis_map;
  !count

let tree_vis_map tree =
  let vis_map = make_vis_map tree in
  look_from_left_right tree vis_map;
  look_from_top_bottom tree vis_map;
  vis_map

let part1 () =
  let ic = open_in "./input.txt" in
  let tree = parse ic in
  let vis_map = tree_vis_map tree in
  print_vis_map vis_map;
  Printf.printf "%d trees are visible\n" (visible_count vis_map)

let part2 () = ()

let () =
  print_endline "-- part1 --";
  part1 ();
  print_endline "\n-- part2 --";
  part2 ()
