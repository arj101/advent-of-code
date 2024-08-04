use std::collections::HashSet;

#[derive(Eq, Hash, PartialEq, Clone, Copy)]
struct Coord {
    x: isize,
    y: isize,
}

fn main() {
    let seen_count = part_2();
    println!("{seen_count}");
}


pub fn part_2() -> usize {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let start = Coord { x: 0, y: 0 };
    let mut rope = vec![start; 10];
    let mut seen = HashSet::new();
    seen.insert(start);

    for line in input.lines() {
        let (dir, amount) = line.split_once(' ').unwrap();
        let amount: u8 = amount.parse().unwrap();

        for _ in 0..amount {
            // move head of the whole rope
            match dir {
                "U" => rope[0].y -= 1,
                "D" => rope[0].y += 1,
                "L" => rope[0].x -= 1,
                "R" => rope[0].x += 1,
                _ => panic!("tried to move in an invalid direction"),
            };

            // move the rest of the rope
            for head_idx in 0..(rope.len())  {
                let tail_idx = head_idx + 1;
                // determine if head and tail are touching
                let diff = Coord {
                    x: rope[head_idx].x - rope[tail_idx].x,
                    y: rope[head_idx].y - rope[tail_idx].y,
                };
                let not_touching = diff.x.abs() > 1 || diff.y.abs() > 1;

                // update tail and insert it into the seen set if needed
                if not_touching {
                    rope[tail_idx].x += diff.x.signum();
                    rope[tail_idx].y += diff.y.signum();
                    if tail_idx == rope.len() - 1 {
                        seen.insert(rope[rope.len() - 1]);
                    }
                }
            }
        }
    }

    seen.len()
}

pub fn part_1() -> usize {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let start = Coord { x: 0, y: 0 };
    let mut head = start;
    let mut tail = start;
    let mut seen = HashSet::new();
    seen.insert(tail);
    
    for line in input.lines() {
        let (dir, amount) = line.split_once(' ').unwrap();
        let amount = amount.parse().unwrap();
        
        for _ in 0..amount {
            // move head
            match dir {
                "U" => head.y -= 1,
                "D" => head.y += 1,
                "L" => head.x -= 1,
                "R" => head.x += 1,
                _ => panic!("tried to move in invalid direction"),
            };

            // determine if head and tail are touching
            let diff = Coord {
                x: head.x - tail.x,
                y: head.y - tail.y,
            };
            let not_touching = diff.x.abs() > 1 || diff.y.abs() > 1;

            // update tail and insert it into the seen set if needed
            if not_touching {
                tail.x += diff.x.signum();
                tail.y += diff.y.signum();
                seen.insert(tail);
            }
        }
    }
    
    seen.len()
}
