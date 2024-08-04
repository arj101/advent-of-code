module Fs = struct
  type node = File of file_desc | Dir of dir_desc
  and file_desc = { name : string; size : int }
  and dir_desc = { name : string; mutable children : node list }

  type nav = { root : dir_desc; path : dir_desc Stack.t }

  let create_nav () =
    { root = { name = "/"; children = [] }; path = Stack.create () }

  let file name size = File { name; size }
  let dir name children = Dir { name; children }

  let current_dir nav =
    if Stack.is_empty nav.path then nav.root else Stack.top nav.path

  let cd_root nav =
    Stack.clear nav.path;
    nav.root

  let cd_back nav =
    Stack.pop nav.path |> ignore;
    current_dir nav

  let is_same_node node1 node2 =
    match (node1, node2) with
    | Dir { name = name1; _ }, Dir { name = name2; _ }
    | File { name = name1; _ }, File { name = name2; _ } ->
        String.equal name1 name2
    | _ -> false

  let rec cd nav dir_name =
    let working_dir = current_dir nav in
    let contains_dir node =
      match node with
      | Dir { name; _ } -> String.equal name dir_name
      | _ -> false
    in
    match List.find_opt contains_dir working_dir.children with
    | Some (Dir desc) ->
        Stack.push desc nav.path;
        Stack.top nav.path
    | None ->
        (* create the directory if it doesnt exist *)
        working_dir.children <- dir dir_name [] :: working_dir.children;
        cd nav dir_name (* cd into it *)
    | _ -> assert false

  let has_child_node nav node =
    let working_dir = current_dir nav in
    List.exists
      (fun dir_node -> is_same_node dir_node node)
      working_dir.children

  let create_node nav node =
    if has_child_node nav node then ()
    else
      let working_dir = current_dir nav in
      working_dir.children <- node :: working_dir.children

  let traverse node =
    let rec print_indent size =
      if size > 0 then (
        print_string "  ";
        print_indent (size - 1))
      else ()
    in

    let rec traverse_aux depth node =
      print_indent depth;
      match node with
      | File { name; size } -> Printf.printf "(%d) %s\n" size name
      | Dir { name; children } ->
          Printf.printf "dir %s [\n" name;
          List.iter (fun node -> traverse_aux (depth + 1) node) children;
          print_indent depth;
          print_endline "]"
    in
    traverse_aux 0 node
end

module InputParser = struct
  let parse_ls channel nav =
    let rec parse_aux channel prev_pos =
      let line = input_line channel in
      let parse_result =
        match String.split_on_char ' ' line with
        | "$" :: _ -> Error ()
        | [ "dir"; dir_name ] ->
            Fs.create_node nav (Fs.dir dir_name []);
            Ok ()
        | [ size; file_name ] ->
            Fs.create_node nav (Fs.file file_name (int_of_string size));
            Ok ()
        | _ -> assert false
      in
      match parse_result with
      | Error _ ->
          In_channel.seek channel prev_pos;
          ()
      | Ok _ -> parse_aux channel (In_channel.pos channel)
    in
    parse_aux channel (In_channel.pos channel)

  let parse_cmd cmd channel nav =
    match cmd with
    | [ "cd"; ".." ] -> Fs.cd_back nav |> ignore
    | [ "cd"; "/" ] -> Fs.cd_root nav |> ignore
    | [ "cd"; dir_name ] -> Fs.cd nav dir_name |> ignore
    | [ "ls" ] -> parse_ls channel nav
    | _ -> assert false

  let parse channel =
    let nav = Fs.create_nav () in
    let rec parse_aux channel =
      let line = input_line channel in
      match String.split_on_char ' ' line with
      | "$" :: cmd ->
          parse_cmd cmd channel nav;
          parse_aux channel
      | _ -> assert false
    in
    try parse_aux channel with End_of_file -> nav
end

let part1 (fs: Fs.nav) =
  let rec compute_sum (dir : Fs.dir_desc) =
    List.fold_left
      (fun (total_size, dirs_sum) node ->
        match node with
        | Fs.File { size; _ } -> (total_size + size, dirs_sum)
        | Fs.Dir desc ->
            let t1, d1 = compute_sum desc in
            if t1 <= 100_000 then (total_size + t1, dirs_sum + t1 + d1)
            else (total_size + t1, dirs_sum + d1))
      (0, 0) dir.children
  in

  let total_size, dirs_size = compute_sum fs.root in
  Printf.printf "Sum: %d\n" dirs_size;
  total_size

let part2 used_space (fs : Fs.nav) =
  let min_required_space = 30_000_000 in
  let fs_capacity = 70_000_000 in
  let min_required_deletion = min_required_space - (fs_capacity - used_space) in
  Printf.printf "%d needs to be freed\n" min_required_deletion;

  let rec smallest_required (dir : Fs.dir_desc) =
    List.fold_left
      (fun (total_size, smallest_size) node ->
        match node with
        | Fs.File { size; _ } -> (total_size + size, smallest_size)
        | Fs.Dir desc ->
            let t1, s1 = smallest_required desc in
            let smallest_size =
              if s1 >= min_required_deletion && s1 < smallest_size then s1
              else if t1 < smallest_size && t1 >= min_required_deletion then t1
              else smallest_size
            in
            (total_size + t1, smallest_size))
      (0, Int.max_int) dir.children
  in

  let _, smallest_size = smallest_required fs.root in
  assert (smallest_size < Int.max_int);
  Printf.printf "Smallest required directory size: %d\n" smallest_size

let () =
  let fs = InputParser.parse (open_in "./input.txt") in
  Fs.traverse (Fs.Dir fs.root);
  print_newline ();

  print_endline "-- part 1 --";
  let total_size = part1 fs in
  print_endline "\n-- part 2 --";
  part2 total_size fs
