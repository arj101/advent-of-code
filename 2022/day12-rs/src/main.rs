use std::collections::HashMap;

type HeightMapChar = Vec<Vec<char>>;
type HeightMap = Vec<Vec<i32>>;

fn height_map(s: &str) -> HeightMap {
    s.lines()
        .map(|line| {
            line.chars()
                .map(|c| {
                    if c != 'E' && c != 'S' {
                        c as i32 - 'a' as i32
                    } else if c == 'E' {
                        'z' as i32 - 'a' as i32
                    } else {
                        0
                    }
                })
                .collect()
        })
        .collect()
}

fn height_map_c(s: &str) -> HeightMapChar {
    s.lines().map(|line| line.chars().collect()).collect()
}

fn start_marker_c(h: &HeightMapChar) -> (usize, usize) {
    for (i, l) in h.iter().enumerate() {
        for (j, height) in l.iter().enumerate() {
            if *height == 'S' {
                return (i, j);
            }
        }
    }
    unreachable!()
}

fn end_marker_c(h: &HeightMapChar) -> (usize, usize) {
    for (i, l) in h.iter().enumerate() {
        for (j, height) in l.iter().enumerate() {
            if *height == 'E' {
                return (i, j);
            }
        }
    }

    unreachable!()
}

fn start_marker(h: &HeightMap) -> (usize, usize) {
    for (i, l) in h.iter().enumerate() {
        for (j, height) in l.iter().enumerate() {
            if *height == 'S' as i32 - 'a' as i32 {
                return (i, j);
            }
        }
    }
    unreachable!()
}

fn end_marker(h: &HeightMap) -> (usize, usize) {
    for (i, l) in h.iter().enumerate() {
        for (j, height) in l.iter().enumerate() {
            if *height == 'z' as i32 - 'a' as i32 + 1 {
                return (i, j);
            }
        }
    }

    unreachable!()
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
struct Pos(i32, i32);

impl From<(usize, usize)> for Pos {
    fn from(value: (usize, usize)) -> Self {
        Pos(value.0 as i32, value.1 as i32)
    }
}

impl From<(i32, i32)> for Pos {
    fn from(value: (i32, i32)) -> Self {
        Pos(value.0, value.1)
    }
}

fn find_path(
    heights: &HeightMap,
    start: (usize, usize),
    end: (usize, usize),
    print: bool,
) -> Option<i32> {
    let map_size_y = heights.len() as i32;
    let map_size_x = heights[0].len() as i32;

    let h = |Pos(x, y): Pos| ((end.0 as i32 - x as i32).abs() + (end.1 as i32 - y as i32).abs());

    let start = Pos::from(start);
    let end = Pos::from(end);

    let mut open_set = vec![];

    open_set.push(Pos::from(start));

    let mut came_from: HashMap<Pos, Pos> = HashMap::new();
    let mut gscore: HashMap<Pos, i32> = HashMap::new();
    let mut fscore: HashMap<Pos, i32> = HashMap::new();

    gscore.insert(start.into(), 0);
    fscore.insert(start.into(), h(start));

    while !open_set.is_empty() {
        let mut current = open_set[0];
        for vals in &open_set {
            if fscore[&vals] < fscore[&current] {
                current = *vals;
            }
        }

        if current == end {
            let mut steps = 0;
            let mut pos = current;

            let mut grid: Vec<Vec<char>> = vec![];

            for _ in 0..map_size_y {
                let mut v = vec![];
                for _ in 0..map_size_x {
                    v.push('.');
                }
                grid.push(v);
            }

            grid[pos.0 as usize][pos.1 as usize] = 'E';

            while pos != start {
                let prev = pos;
                pos = came_from[&pos];
                steps += 1;

                if !print {
                    continue;
                }
                let mut c = '.';

                if pos.1 < prev.1 {
                    c = '>';
                }
                if pos.1 > prev.1 {
                    c = '<';
                }
                if pos.0 < prev.0 {
                    c = 'v';
                }
                if pos.0 > prev.0 {
                    c = '^';
                }

                grid[pos.0 as usize][pos.1 as usize] = c;
            }
            if pos == start {
                grid[pos.0 as usize][pos.1 as usize] = 'S';
            }

            if print {
                println!();

                for line in grid {
                    println!("{}", line.iter().collect::<String>());
                }

                println!();
            }

            return Some(steps);
        }

        let mut neighbours = vec![];

        if current.0 > 0 {
            neighbours.push((current.0 - 1, current.1));
        }

        if current.1 > 0 {
            neighbours.push((current.0, current.1 - 1));
        }

        if current.0 < map_size_y - 1 {
            neighbours.push((current.0 + 1, current.1));
        }

        if current.1 < map_size_x - 1 {
            neighbours.push((current.0, current.1 + 1));
        }

        open_set = open_set
            .iter()
            .filter(|x| **x != current)
            .copied()
            .collect();

        for n in neighbours {
            let n = Pos::from(n);

            if (heights[n.0 as usize][n.1 as usize]
                - heights[current.0 as usize][current.1 as usize])
                > 1
            {
                continue;
            }

            let tgscore = gscore[&current] + 1;

            if tgscore < *gscore.get(&n).unwrap_or(&std::i32::MAX) {
                came_from.insert(n, current);
                gscore.insert(n, tgscore);
                fscore.insert(n, tgscore + h(n));

                if !open_set.contains(&n) {
                    open_set.push(n);
                }
            }
        }
    }

    None
}

fn part1(map: &str) {
    let hmapc = height_map_c(map);
    let start = start_marker_c(&hmapc);
    let end = end_marker_c(&hmapc);

    let hmap = height_map(map);

    println!("start: {start:?}");
    println!("end: {end:?}");
    let step_count = find_path(&hmap, start, end, true).unwrap();
    println!("Took {step_count} steps");
}

fn part2(map: &str) {
    let hmapc = height_map_c(map);
    let hmap = height_map(map);

    let end = end_marker_c(&hmapc);
    // let mut start_poses = vec!();

    let mut start = (0, 0);
    let mut shortest = std::i32::MAX;

    //TODO: come up with a better solution
    for (i, l) in hmapc.iter().enumerate() {
        for (j, height) in l.iter().enumerate() {
            if *height != 'a' {
                continue;
            }
            // start_poses.push((i, j));
            if let Some(steps) = find_path(&hmap, (i, j), end, false) {
                if steps < shortest {
                    shortest = steps;
                    start = (i, j);
                }
            }
        }
    }

    find_path(&hmap, start, end, true);
    println!("Shortest = {shortest} steps");
    println!("Start = {start:?}");
}

fn main() {
    let s = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    println!("-- Part 1 --");
    part1(&s);
    println!();
    println!("-- Part 2 --");
    part2(&s);
}
