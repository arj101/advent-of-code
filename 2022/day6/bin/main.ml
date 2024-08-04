let read_len_as_str channel length =
  let rec aux buf remaining =
    if remaining <= 0 then Buffer.contents buf
    else (
      Buffer.add_char buf (input_char channel);
      aux buf (remaining - 1))
  in
  let buf = Buffer.create length in
  aux buf length

type search_final_result = Found | NotFound

(* for part 1 *)
module PacketMarker = struct
  type t = char array
  type search_result = Unique | Duplicate

  exception DuplicateExn (* exception as control flow >:) *)

  let create () : t = [||]

  let push_char (marker_finder : t) c =
    let push_char_aux marker_finder c =
      if Array.length marker_finder < 4 then Array.append marker_finder [| c |]
      else (
        (*shift elements to left*)
        for i = 0 to 2 do
          Array.set marker_finder i marker_finder.(i + 1)
        done;
        Array.set marker_finder 3 c;
        marker_finder)
    in
    let marker_finder = push_char_aux marker_finder c in
    if Array.length marker_finder < 4 then (marker_finder, Duplicate)
    else
      try
        Array.fold_left
          (fun visited elt ->
            match Array.find_opt (fun elt2 -> elt == elt2) visited with
            | None -> Array.append visited [| elt |]
            | Some _ -> raise DuplicateExn)
          [||] marker_finder
        |> ignore;
        (marker_finder, Unique)
      with DuplicateExn -> (marker_finder, Duplicate)
end

(* for part 2 *)
module MessageMarker = struct
  type order_elt = { vpos : int; value : int }

  type t = {
    mutable vpos : int;
    max_length : int;
    order : order_elt Queue.t;
    elements : bool array;
  }

  type state = Unique | PartialOrNonUnique

  let create max_length =
    {
      vpos = 0;
      max_length;
      order = Queue.create ();
      elements = Array.make 26 false;
    }

  let hash c = Char.code c - Char.code 'a'
  let unhash h = Char.chr (h + Char.code 'a')

  (* let print_order marker = *)
  (*   Seq.iter (fun {value} -> print_char (unhash value)) (Queue.to_seq marker.order) *)

  let pop marker =
    let { value = rm_elt; _ } = Queue.pop marker.order in
    Array.set marker.elements rm_elt false;
    unhash rm_elt

  let pop_opt marker =
    match Queue.take_opt marker.order with
    | Some { value; _ } ->
        Array.set marker.elements value false;
        Some value
    | None -> None

  let push_unique marker c =
    let hashed = hash c in
    if marker.elements.(hashed) then
      let rec pop_duplicate () =
        match pop_opt marker with
        | Some value when value == hashed -> ()
        | None -> ()
        | _ -> pop_duplicate ()
      in
      pop_duplicate ()
    else ();
    Queue.push { value = hashed; vpos = marker.vpos } marker.order;
    Array.set marker.elements hashed true

  let incr_vpos marker = marker.vpos <- marker.vpos + 1

  let push marker c =
    push_unique marker c;
    incr_vpos marker;

    assert (Queue.length marker.order <= marker.max_length);
    if Queue.length marker.order == marker.max_length then Unique
    else PartialOrNonUnique
end

let part1 () =
  let ic = open_in "./input.txt" in
  let marker = PacketMarker.create () in
  let rec find_marker channel marker =
    try
      match input_char channel with
      | 'a' .. 'z' as c -> (
          match PacketMarker.push_char marker c with
          | f, Duplicate -> find_marker channel f
          | _, Unique -> Found)
      | '\n' -> NotFound
      | _ -> assert false
    with End_of_file -> NotFound
  in
  let marker = find_marker ic marker in
  match marker with
  | NotFound -> assert false
  | Found ->
      let pos = In_channel.pos ic |> Int64.to_int in
      Printf.printf "Packet marker at %d\n" pos;
      In_channel.seek ic (Int64.of_int (pos - 4));
      Printf.printf "Marker: %s\n" (read_len_as_str ic 4)

let part2 () =
  let ic = open_in "./input.txt" in
  let rec find_marker channel marker =
    match input_char channel with
    | 'a' .. 'z' as c -> (
        match MessageMarker.push marker c with
        | Unique -> Found
        | PartialOrNonUnique -> find_marker channel marker)
    | '\n' -> NotFound
    | _ -> assert false
  in
  match find_marker ic (MessageMarker.create 14) with
  | Found ->
      let pos = In_channel.pos ic |> Int64.to_int in
      Printf.printf "Message marker at %d\n" pos;
      In_channel.seek ic (Int64.of_int (pos - 14));
      Printf.printf "Marker: %s\n" (read_len_as_str ic 14)
  | NotFound -> assert false

let () =
  print_endline "-- part 1 --";
  part1 ();
  print_endline "\n-- part 2 --";
  part2 ()
