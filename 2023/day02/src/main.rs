use regex::Regex;

fn main() {
    let input = include_str!("./input.txt");
    part1(input);
    part2(input);
}

fn part1(input: &str) {
    const RED_COUNT: i8 = 12;
    const GREEN_COUNT: i8 = 13;
    const BLUE_COUNT: i8 = 14;

    let games = parse_games(input);
    let possible_ids = games.iter().map(|game| {
        for round in game.rounds.iter() {
            if round.red > RED_COUNT || round.green > GREEN_COUNT || round.blue > BLUE_COUNT {
                return 0;
            }
        }
        return game.id;
    });
    let id_sum: i32 = possible_ids.map(|i| i as i32).sum();
    println!("Part1: {}", id_sum)
}

fn part2(input: &str) {
    let games = parse_games(input);
    let power_sum: i32 = games.iter().map(|game| game.power()).sum();
    println!("Part2: {}", power_sum);
}

#[derive(Debug)]
struct Game {
    id: i8,
    rounds: Vec<Round>,
}

impl Game {
    fn power(&self) -> i32 {
        let mut min_red: i32 = 0;
        let mut min_green: i32 = 0;
        let mut min_blue: i32 = 0;
        for round in self.rounds.iter() {
            min_red = std::cmp::max(min_red, round.red as i32);
            min_green = std::cmp::max(min_green, round.green as i32);
            min_blue = std::cmp::max(min_blue, round.blue as i32);
        }
        return min_red * min_green * min_blue;
    }
}

#[derive(Debug)]
struct Round {
    blue: i8,
    red: i8,
    green: i8,
}

fn parse_games(input: &str) -> Vec<Game> {
    let number_re = Regex::new(r"[0-9]+").unwrap();
    let games = input.split("\n").map(|line| {
        if line.trim().len() == 0 {
            return Game {
                id: 0,
                rounds: vec![],
            };
        }

        let split = line.split(":").collect::<Vec<&str>>();
        let p1 = split[0];
        let p2 = split[1];
        let game_id_match = number_re.find(p1).unwrap();
        let game_id = &p1[game_id_match.start()..game_id_match.end()]
            .parse::<i8>()
            .unwrap();

        let rounds = p2.trim().split(";").map(|round_text| {
            let mut blue = 0;
            let mut red = 0;
            let mut green = 0;

            for color_text in round_text.split(",") {
                let color_match = number_re.find(color_text).unwrap();
                let color_count = color_text[color_match.start()..color_match.end()]
                    .parse::<i8>()
                    .unwrap();
                if color_text.contains("blue") {
                    blue = color_count;
                } else if color_text.contains("red") {
                    red = color_count;
                } else if color_text.contains("green") {
                    green = color_count;
                }
            }
            return Round {
                blue: blue,
                red: red,
                green: green,
            };
        });
        return Game {
            id: *game_id,
            rounds: rounds.collect::<Vec<Round>>(),
        };
    });
    return games.collect::<Vec<Game>>();
}
