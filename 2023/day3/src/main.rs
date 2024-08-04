fn main() {
    let s = std::fs::read_to_string("./input.txt").unwrap();
    let line_len = s.lines().next().unwrap().len();
    println!("{line_len}");
    

    let s = s.chars().filter(|c| *c != '\n' && *c != '\t' && *c != '\r').collect::<Vec<char>>();
    println!("{}", s.len() );
    let num_lines = s.len() / line_len;

    let char_at = |x: usize, y: usize| s[(line_len as usize * y + x) as usize];

    let neighbours: Vec<[isize; 2]> = vec![
        [-1, 0],
        [-1, -1],
        [0, -1],
        [1, -1],
        [1, 0],
        [1, 1],
        [0, 1],
        [-1, 1],
    ];

    let mut sum = 0;
    for j in 0..num_lines {

        let mut num = 0;
        let mut is_part = false;

        for i in 0..line_len {
            let is_number = char_at(i, j).is_digit(10);

            if is_number {
                num = char_at(i, j).to_string().parse::<u32>().unwrap() + num * 10;
                print!("{num} -> ");

                'inner: for [dx, dy] in &neighbours {
                    let x = i as isize + dx;
                    let y = j as isize + dy;
                    if x < 0 || y < 0 || x >= line_len as isize || y >= num_lines as isize { continue 'inner; }
                    if (char_at(x as usize, y as usize) != '.' && !char_at(x as usize, y as usize).is_digit(10)) {
                        is_part = true;
                    }
                }
            } else {
                if is_part {
                    sum += num;
                        // println!("{}", num);;
                }

                if num > 0 { println!(" {num} {is_part}; sum: {sum}") }
                if is_part {


                    is_part = false;
                }
                num = 0;
            }
        }

        if is_part {
            sum += num;
                if num > 0 { println!(" {num} {is_part}; sum: {sum}") }
        }
    }
    eprintln!("Sum = {sum}");
}
