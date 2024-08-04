
let () = 
        let ic = open_in "./input.txt" in

        let rec max_calories channel curr_max =
                try 
                   let line = input_line channel in
                   let calorie0 = int_of_string line in
                   let rec elf_calorie channel accum =
                           (try
                                   let line = input_line channel in
                                  ( match int_of_string_opt line with
                                   | Some c -> elf_calorie channel (accum + c)
                                   | None -> accum)
                        with End_of_file -> accum) in
                   let max_calorie = max curr_max (elf_calorie channel calorie0) in max_calories channel max_calorie
                with End_of_file -> curr_max
        in

        print_endline "Hello, World!"


