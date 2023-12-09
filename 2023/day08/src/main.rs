use std::collections::HashMap;

use regex::Regex;

fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let map = parse_map(input);
    let result = map.steps_to("AAA", "ZZZ");
    println!("Part 1: {:?}", result);
}

fn part2(input: &str) {
    let map = parse_map(input);
    let ends_with_a = map.get_nodes_that_end_with_a();

    let mut index = 0;
    let mut steps = 0;
    let mut current_steps = ends_with_a;
    loop {
        let all_end_with_z = current_steps.iter().all(|s| s.ends_with("Z"));
        if all_end_with_z {
            break;
        }
        if index == map.steps.len() {
            index = 0;
        }

        let next_step = map.steps[index];
        current_steps = current_steps
            .iter()
            .map(|step| {
                let node = map.adjacency_map.get(step).unwrap();
                if next_step == 'R' {
                    node.right.to_string()
                } else {
                    node.left.to_string()
                }
            })
            .collect::<Vec<_>>();

        index += 1;
        steps += 1;
    }

    println!("Part 2: {:?}", steps);
}

#[derive(Debug)]
struct Map {
    steps: Vec<char>,
    adjacency_map: HashMap<String, Node>,
}

impl Map {
    fn steps_to(&self, start: &str, end: &str) -> i32 {
        let mut index = 0;
        let mut steps = 0;
        let mut current_step = start.to_string();
        loop {
            if current_step == end {
                return steps;
            }
            if index == self.steps.len() {
                index = 0;
            }
            let node = self.adjacency_map.get(&current_step).unwrap();
            let next_step = self.steps[index];
            current_step = if next_step == 'R' {
                node.right.to_string()
            } else {
                node.left.to_string()
            };

            index += 1;
            steps += 1;
        }
    }

    fn get_nodes_that_end_with_a(&self) -> Vec<String> {
        self.adjacency_map
            .keys()
            .filter(|k| k.ends_with("A"))
            .map(|k| k.to_string())
            .collect::<Vec<_>>()
    }
}

#[derive(Debug)]
struct Node {
    left: String,
    right: String,
}

fn parse_map(input: &str) -> Map {
    let splits = input.split("\n").collect::<Vec<_>>();
    let steps = splits[0];
    let node_re = Regex::new(r"[0-9A-Z][0-9A-Z][0-9A-Z]").unwrap();
    let adjacency_map = splits[1..]
        .iter()
        .filter(|s| !s.trim().is_empty())
        .map(|s| node_re.find_iter(s).map(|m| m.as_str()).collect::<Vec<_>>())
        .map(|m| {
            (
                m[0].to_string(),
                Node {
                    left: m[1].to_string(),
                    right: m[2].to_string(),
                },
            )
        })
        .collect::<Vec<_>>();

    return Map {
        steps: steps.chars().collect::<Vec<_>>(),
        adjacency_map: HashMap::from_iter(adjacency_map),
    };
}
