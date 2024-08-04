

type height_map = int list list

let height_map_of ic = 
    let rec aux (acc: int list list) ic = 
        try
            let line = input_line ic in
            aux ((List.map int_of_char (line |> String.to_seq |> List.of_seq)) :: acc) ic
        with End_of_file ->
            acc
    in
    aux [] ic
        


let () = 
    let file = open_in "./input.txt" in

    print_endline "hello";
    print_endline "world"
