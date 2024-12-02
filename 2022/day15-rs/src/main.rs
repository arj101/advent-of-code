use std::{
    collections::{HashMap, HashSet},
    ops::{Add, Div, Mul, Sub},
};

#[derive(Debug, Clone, Copy)]
struct SensorBeaconInfo {
    sensor: (i32, i32),
    beacon: (i32, i32),
}

#[derive(Debug, Clone, Copy)]
struct SensorInfo {
    sensor: (i32, i32),
    range: i32,
}
impl SensorInfo {
    fn in_range(&self, x: i32, y: i32) -> bool {
        (self.sensor.0 - x).abs() + (self.sensor.1 - y).abs() <= self.range
    }
}

impl From<SensorBeaconInfo> for SensorInfo {
    fn from(value: SensorBeaconInfo) -> Self {
        Self {
            sensor: value.sensor,
            range: (value.sensor.0 - value.beacon.0).abs()
                + (value.sensor.1 - value.beacon.1).abs(),
        }
    }
}

fn parse_sensor_line(s: &str) -> SensorBeaconInfo {
    let parts = s.split_whitespace().collect::<Vec<&str>>();

    let sensor_x = parts[2]
        .split("=")
        .map(|x| x.split(",").nth(0))
        .nth(1)
        .unwrap()
        .unwrap();

    let sensor_y = parts[3]
        .split("=")
        .map(|x| x.split(":").nth(0))
        .nth(1)
        .unwrap()
        .unwrap();

    let beacon_x = parts[8]
        .split("=")
        .map(|x| x.split(",").nth(0))
        .nth(1)
        .unwrap()
        .unwrap();

    let beacon_y = parts[9].split("=").nth(1).unwrap();

    SensorBeaconInfo {
        sensor: (sensor_x.parse().unwrap(), sensor_y.parse().unwrap()),
        beacon: (beacon_x.parse().unwrap(), beacon_y.parse().unwrap()),
    }
}

fn part1(s: &str) {
    const SEARCH_Y: i32 = 200_0000;

    let sensor_beacons = s
        .lines()
        .map(parse_sensor_line)
        .collect::<Vec<SensorBeaconInfo>>();
    let sensors = sensor_beacons
        .iter()
        .map(|s| s.clone().into())
        .collect::<Vec<SensorInfo>>();

    let beacons = sensor_beacons
        .iter()
        .filter(|SensorBeaconInfo { sensor: _, beacon }| beacon.1 == SEARCH_Y)
        .map(|SensorBeaconInfo { sensor: _, beacon }| beacon.0)
        .collect::<HashSet<i32>>();

    //TODO: use something else for keeping track of visited position

    let mut empty_poses: HashSet<i32> = HashSet::new();

    for s in &sensors {
        let dy = (s.sensor.1 - SEARCH_Y).abs();

        if dy > s.range {
            continue;
        }
        let dx = s.range - dy;

        for x in (s.sensor.0 - dx)..=(s.sensor.0 + dx) {
            if beacons.contains(&x) {
                continue;
            }
            empty_poses.insert(x);
        }
    }

    println!("Cannot contain beacons at {} positions", empty_poses.len());
}

#[derive(PartialEq, Debug, Clone, Copy)]
struct Vec2 {
    x: f32,
    y: f32,
}

impl Vec2 {
    fn dot(&self, other: &Self) -> f32 {
        self.x * other.x + self.y * other.y
    }

    fn cross(&self, other: &Self) -> f32 {
        self.x * other.y - self.y * other.x
    }

    fn length(&self) -> f32 {
        (self.x * self.x + self.y * self.y).sqrt()
    }

    fn normalize(&self) -> Self {
        let len = self.length();
        Self {
            x: self.x / len,
            y: self.y / len,
        }
    }
}

impl Add for Vec2 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Div<f32> for Vec2 {
    type Output = Self;

    fn div(self, other: f32) -> Self {
        Self {
            x: self.x / other,
            y: self.y / other,
        }
    }
}

impl Sub for Vec2 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Mul<f32> for Vec2 {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        Self {
            x: self.x * other,
            y: self.y * other,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
struct Line {
    start: Vec2,
    end: Vec2,
    intersect_count: i32,
    normal: Vec2,
    sensor_idx: usize,
}

impl Line {
    fn from_to(start: (i32, i32), end: (i32, i32), sensor_idx: usize) -> Self {
        Self {
            start: Vec2 {
                x: start.0 as f32,
                y: start.1 as f32,
            },
            end: Vec2 {
                x: end.0 as f32,
                y: end.1 as f32,
            },
            intersect_count: 0,
            normal: Vec2 {
                x: (end.1 - start.1) as f32,
                y: (start.0 - end.0) as f32,
            }
            .normalize(),
            sensor_idx,
        }
    }

    fn slope(&self) -> f32 {
        (self.end.y - self.start.y) / (self.end.x - self.start.x)
    }

    fn intersects(&self, other: &Line) -> Option<(i32, i32)> {
        let d = self.normal.dot(&other.normal);
        let dir1 = (self.start - self.end);
        let dir2 = (other.start - other.end);

        if dir1.dot(&dir2) != 0. || dir1.dot(&dir2) < 0. {
            return None;
        }

        let mid1 = (self.start + self.end) / 2.;
        let mid2 = (other.start + other.end) / 2.;

        let mid1len = (self.end - self.start).length() / 2.;
        let mid2len = (other.end - other.start).length() / 2.;

        let d1 = (mid2 - mid1).dot(&self.normal);
        let d2 = (mid1 - mid2).dot(&other.normal);

        if d1.abs() <= mid1len && d2.abs() <= mid2len {
            let p = mid1 + other.normal * d2;
            return Some((p.x as i32, p.y as i32));
        }

        return None;
    }
}

fn manhattan_dist(p1: (i32, i32), p2: (i32, i32)) -> i32 {
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs()
}

fn part2(s: &str) {
    let sensor_beacons = s
        .lines()
        .map(parse_sensor_line)
        .collect::<Vec<SensorBeaconInfo>>();
    let sensors = sensor_beacons
        .iter()
        .map(|s| s.clone().into())
        .collect::<Vec<SensorInfo>>();

    let mut lines: Vec<Line> = vec![];

    let mut points = vec![];

    const UPPER_BOUND: i32 = 4_000_000;
    let mut insert_if_existnt = |pt| {
        points.push(pt);
    };

    for (i, s) in sensors.iter().enumerate() {
        let dx = s.range + 1;

        for x in (s.sensor.0 - dx)..=(s.sensor.0 + dx) {
            let dy = s.range + 1 - (x - s.sensor.0).abs();

            if x < 0 || x > UPPER_BOUND {
                continue;
            }

            let p1 = (x, s.sensor.1 + dy);
            let p2 = (x, s.sensor.1 - dy);

            if p1.1 >= 0 && p1.1 <= UPPER_BOUND {
                insert_if_existnt(p1);
            }
            if p2.1 >= 0 && p2.1 <= UPPER_BOUND {
                insert_if_existnt(p2);
            }
        }

        // let l1 = Line::from_to(
        //     (s.sensor.0 as i32, s.sensor.1 - dy as i32),
        //     (s.sensor.0 - dx as i32, s.sensor.1 as i32),
        //     i,
        // );

        // let l2 = Line::from_to(
        //     (s.sensor.0 - dx as i32, s.sensor.1 as i32),
        //     (s.sensor.0 as i32, s.sensor.1 + dy as i32),
        //     i,
        // );

        // let l3 = Line::from_to(
        //     (s.sensor.0 as i32, s.sensor.1 + dy as i32),
        //     (s.sensor.0 + dx as i32, s.sensor.1 as i32),
        //     i,
        // );

        // let l4 = Line::from_to(
        //     (s.sensor.0 + dx as i32, s.sensor.1 as i32),
        //     (s.sensor.0 as i32, s.sensor.1 - dy as i32),
        //     i,
        // );

        // lines.push(l1);
        // lines.push(l2);
        // lines.push(l3);
        // lines.push(l4);
    }

    // for i in 0..lines.len() {
    //     for j in 0..lines.len() {
    //         if let Some(pt) = lines[i].intersects(&lines[j]) {
    //             lines[i].intersect_count += 1;
    //             lines[j].intersect_count += 1;

    //             if let Some(int_count) = points.get(&pt) {
    //                 points.insert(pt, int_count + 1);
    //             } else {
    //                 points.insert(pt, 1);
    //             }
    //         }
    //     }
    // }

    // for l in &lines {
    //     println!(
    //         "Sensor: {}, intersects: {}",
    //         l.sensor_idx, l.intersect_count,
    //     )
    // }

    for pt in points {
        if sensors.iter().any(|s| s.in_range(pt.0, pt.1)) {
            continue;
        }
        let pt_x = pt.0 as usize;
        let pt_y = pt.1 as usize;
        println!(
            "No sensor covers {:?}, tuning frequency = {}",
            pt,
            pt_x * 4_000_000 + pt_y,
        );
    }
    // println!("in range :(");
}

fn main() {
    let s = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("-- Part 1 --");
    part1(&s);
    println!("-- Part 2 --");
    part2(&s);
}
