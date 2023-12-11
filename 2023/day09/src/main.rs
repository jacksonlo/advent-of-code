fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let histories = parse_histories(input);
    let sum = histories.iter().map(|h| h.next_value()).sum::<i32>();
    println!("Part 1: {:?}", sum);
}

fn part2(input: &str) {
    let histories = parse_histories(input);
    let sum = histories.iter().map(|h| h.previous_value()).sum::<i32>();
    println!("Part 2: {:?}", sum);
}

#[derive(Debug)]
struct History {
    values: Vec<i32>,
}

impl History {
    fn next_value(&self) -> i32 {
        let is_zero = !self.values.iter().any(|&d| d != 0);
        if is_zero {
            return 0;
        }

        let differences = self.differences();
        let differences_next_value = History {
            values: differences,
        }
        .next_value();
        let next_value = self.values.last().unwrap() + differences_next_value;
        return next_value;
    }

    fn previous_value(&self) -> i32 {
        let is_zero = !self.values.iter().any(|&d| d != 0);
        if is_zero {
            return 0;
        }

        let differences = self.differences();
        let differences_prev_value = History {
            values: differences,
        }
        .previous_value();
        let previous_value = self.values.first().unwrap() - differences_prev_value;
        return previous_value;
    }

    fn differences(&self) -> Vec<i32> {
        return self
            .values
            .windows(2)
            .map(|w| w[1] - w[0])
            .collect::<Vec<_>>();
    }
}

fn parse_histories(input: &str) -> Vec<History> {
    let mut histories = Vec::new();
    for line in input.lines() {
        let mut history = History { values: Vec::new() };
        for value in line.split(" ") {
            history.values.push(value.parse().unwrap());
        }
        histories.push(history);
    }
    histories
}
