type ScanVertex = (i32, i32);
type ScanLine = Vec<ScanVertex>;

fn parse_scan_line(line: &str) -> ScanLine {
    line.split("->")
        .map(|pos| {
            let mut nums = pos.split(",");
            (
                nums.next().unwrap().trim().parse().unwrap(),
                nums.next().unwrap().trim().parse().unwrap(),
            )
        })
        .collect()
}

struct MapMetadata {
    source_x_idx: i32,
    source_y_idx: i32,

    low_x: i32,
    low_y: i32,
    top_x: i32,
    top_y: i32,
}

fn draw_scanlines(s: &str) -> (Vec<Vec<char>>, MapMetadata) {
    let scanlines: Vec<ScanLine> = s.lines().map(parse_scan_line).collect();

    let mut low_x = i32::MAX;
    let low_y = 0;
    let mut top_x = 500;
    let mut top_y = 0;

    for l in &scanlines {
        for (x, y) in l {
            if *x > top_x {
                top_x = *x;
            }
            if *y > top_y {
                top_y = *y;
            }

            if *x < low_x {
                low_x = *x;
            }
        }
    }

    let size_x = top_x - low_x + 1;
    let size_y = top_y - low_y + 1;

    let mut grid = vec![];
    let mut line = vec![];
    for _ in 0..size_x {
        line.push('.');
    }
    for _ in 0..size_y {
        grid.push(line.clone());
    }

    grid[0][500 - low_x as usize] = '+';

    for l in scanlines {
        for i in 0..l.len() - 1 {
            let (x1, y1) = l[i];
            let (x2, y2) = l[i + 1];

            let dx = x2 - x1;
            let dy = y2 - y1;
            if dx.abs() > 0 && dy.abs() > 0 {
                unreachable!()
            }

            if dx.abs() > 0 {
                let yidx = y1 - low_y;
                for off_x in 0..=dx.abs() {
                    let xidx = (off_x * dx.signum() + x1) - low_x;
                    grid[yidx as usize][xidx as usize] = '#';
                }
            } else if dy.abs() > 0 {
                let xidx = x1 - low_x;
                for off_y in 0..=dy.abs() {
                    let yidx = (off_y * dy.signum() + y1) - low_y;
                    grid[yidx as usize][xidx as usize] = '#';
                }
            }
        }
    }

    (
        grid,
        MapMetadata {
            source_x_idx: 500 - low_x,
            source_y_idx: 0,

            top_y,
            top_x,
            low_x,
            low_y,
        },
    )
}

fn draw_scanlines2(s: &str) -> (Vec<Vec<char>>, MapMetadata) {
    let scanlines: Vec<ScanLine> = s.lines().map(parse_scan_line).collect();

    let mut low_x = i32::MAX;
    let low_y = 0;
    let mut top_x = 500;
    let mut top_y = 0;

    for l in &scanlines {
        for (x, y) in l {
            if *x > top_x {
                top_x = *x;
            }
            if *y > top_y {
                top_y = *y;
            }

            if *x < low_x {
                low_x = *x;
            }
        }
    }

    top_y += 1;
    let size_y = top_y - low_y + 1;
    low_x = 500 - size_y;
    top_x = 500 + size_y;

    let size_x = top_x - low_x + 1;

    let mut grid = vec![];
    let mut line = vec![];
    for _ in 0..size_x {
        line.push('.');
    }
    for _ in 0..size_y {
        grid.push(line.clone());
    }

    grid[0][500 - low_x as usize] = '+';

    for l in scanlines {
        for i in 0..l.len() - 1 {
            let (x1, y1) = l[i];
            let (x2, y2) = l[i + 1];

            let dx = x2 - x1;
            let dy = y2 - y1;
            if dx.abs() > 0 && dy.abs() > 0 {
                unreachable!()
            }

            if dx.abs() > 0 {
                let yidx = y1 - low_y;
                for off_x in 0..=dx.abs() {
                    let xidx = (off_x * dx.signum() + x1) - low_x;
                    grid[yidx as usize][xidx as usize] = '#';
                }
            } else if dy.abs() > 0 {
                let xidx = x1 - low_x;
                for off_y in 0..=dy.abs() {
                    let yidx = (off_y * dy.signum() + y1) - low_y;
                    grid[yidx as usize][xidx as usize] = '#';
                }
            }
        }
    }

    (
        grid,
        MapMetadata {
            source_x_idx: 500 - low_x,
            source_y_idx: 0,

            top_y,
            top_x,
            low_x,
            low_y,
        },
    )
}

enum SandDropResult {
    CameToRest,
    FellDown,
}

enum SandDropResult2 {
    OnSource,
    BelowSource,
}

fn drop_sand(
    map: &mut Vec<Vec<char>>,
    (source_x, source_y): (i32, i32),
    y_thresh: i32,
) -> SandDropResult {
    let mut pos = (source_x, source_y);

    loop {
        let possible_poses = vec![
            (pos.0, pos.1 + 1),
            (pos.0 - 1, pos.1 + 1),
            (pos.0 + 1, pos.1 + 1),
        ];

        let mut moved = false;
        for (nx, ny) in possible_poses {
            if nx < 0 || ny < 0 || nx as usize >= map[0].len() || ny as usize >= map.len() {
                map[pos.1 as usize][pos.0 as usize] = '.';
                return SandDropResult::FellDown;
            }

            if map[ny as usize][nx as usize] == '.' {
                map[pos.1 as usize][pos.0 as usize] = '.';
                pos = (nx, ny);
                map[pos.1 as usize][pos.0 as usize] = 'o';
                moved = true;
                break;
            }
        }

        if pos.1 > y_thresh {
            return SandDropResult::FellDown;
        }
        if !moved {
            return SandDropResult::CameToRest;
        }
    }
}

fn drop_sand2(
    map: &mut Vec<Vec<char>>,
    (source_x, source_y): (i32, i32),
    y_thresh: i32,
) -> SandDropResult2 {
    let mut pos = (source_x, source_y);
    map[pos.1 as usize][pos.0 as usize] = 'o';

    loop {
        let possible_poses = vec![
            (pos.0, pos.1 + 1),
            (pos.0 - 1, pos.1 + 1),
            (pos.0 + 1, pos.1 + 1),
        ];

        let mut moved = false;
        for (nx, ny) in possible_poses {
            if ny > y_thresh {
                return SandDropResult2::BelowSource;
            }

            if map[ny as usize][nx as usize] == '.' {
                map[pos.1 as usize][pos.0 as usize] = '.';
                pos = (nx, ny);
                map[pos.1 as usize][pos.0 as usize] = 'o';
                moved = true;
                break;
            }
        }

        if !moved {
            break {
                if pos.1 == source_y && pos.0 == source_x {
                    SandDropResult2::OnSource
                } else {
                    SandDropResult2::BelowSource
                }
            };
        }
    }
}

fn part1(s: &str) {
    let (
        mut map,
        MapMetadata {
            source_x_idx,
            source_y_idx,
            low_x,
            low_y,
            top_x,
            top_y,
        },
    ) = draw_scanlines(s);

    let mut drop_count = 0;

    while let SandDropResult::CameToRest = drop_sand(&mut map, (source_x_idx, source_y_idx), top_y)
    {
        drop_count += 1;
    }
    for line in map.iter().map(|l| l.iter().collect::<String>()) {
        println!("{line}");
    }

    println!();
    println!("drop count: {drop_count}");
}

fn part2(s: &str) {
    let (
        mut map,
        MapMetadata {
            source_x_idx,
            source_y_idx,
            low_x,
            low_y,
            top_x,
            top_y,
        },
    ) = draw_scanlines2(s);

    let mut drop_count = 0;

    while let SandDropResult2::BelowSource =
        drop_sand2(&mut map, (source_x_idx, source_y_idx), top_y)
    {
        drop_count += 1;
    }
    drop_count += 1;

    for line in map.iter().map(|l| l.iter().collect::<String>()) {
        println!("{line}");
    }

    println!();
    println!("drop count: {drop_count}");
}

fn main() {
    let s = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("-- Part 1 --");
    part1(&s);
    println!("-- Part 2 --");
    part2(&s);
}
