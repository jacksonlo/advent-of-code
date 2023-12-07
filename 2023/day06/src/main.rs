fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let races = parse_races(input);
    let result = races
        .iter()
        .map(|r| r.number_of_ways_to_beat())
        .fold(1, |acc, e| acc * e);
    println!("Part 1: {}", result);
}

fn part2(input: &str) {
    let splits = input.split("\n").collect::<Vec<_>>();
    let time = splits[0].split(":").collect::<Vec<_>>()[1]
        .replace(" ", "")
        .parse::<u64>()
        .unwrap();
    let distance = splits[1].split(":").collect::<Vec<_>>()[1]
        .replace(" ", "")
        .parse::<u64>()
        .unwrap();
    let result = Race { time, distance }.number_of_ways_to_beat();
    println!("Part 2: {}", result);
}

#[derive(Debug)]
struct Race {
    time: u64,
    distance: u64,
}

impl Race {
    fn number_of_ways_to_beat(&self) -> u64 {
        let mut number_of_ways = 0;
        for hold_time in 0..self.time {
            let velocity = hold_time;
            let remaining_time = self.time - hold_time;
            let distance_travelled = velocity * remaining_time;
            if distance_travelled > self.distance {
                number_of_ways += 1;
            }
        }
        return number_of_ways;
    }
}

fn parse_races(input: &str) -> Vec<Race> {
    let splits = input.split("\n").collect::<Vec<_>>();

    let times = splits[0].split(" ").collect::<Vec<_>>()[1..]
        .iter()
        .filter(|s| !s.trim().is_empty())
        .map(|s| s.trim().parse::<u64>().unwrap())
        .collect::<Vec<_>>();
    let distances = splits[1].split(" ").collect::<Vec<_>>()[1..]
        .iter()
        .filter(|s| !s.trim().is_empty())
        .map(|s| s.trim().parse::<u64>().unwrap())
        .collect::<Vec<_>>();

    let mut races = Vec::new();
    for (time, distance) in times.iter().zip(distances.iter()) {
        races.push(Race {
            time: *time,
            distance: *distance,
        });
    }
    return races;
}
