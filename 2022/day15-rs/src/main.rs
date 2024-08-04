use std::collections::HashSet;

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

        if dy > s.range { continue }
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

fn main() {
    let s = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("-- Part 1 --");
    part1(&s);
    println!("-- Part 2 --");
}
