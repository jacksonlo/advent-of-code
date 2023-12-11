use std::collections::HashMap;

use num_integer::lcm;
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
    let a_locations = map.get_nodes_that_end_with_a();
    let z_locations = a_locations.iter().map(|a| map.find_z_location(&a));
    let steps = z_locations
        .map(|z| z.step_index as u64)
        .reduce(|a, b| lcm(a, b))
        .unwrap();
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
        let mut result = self
            .adjacency_map
            .keys()
            .filter(|k| k.ends_with("A"))
            .map(|k| k.to_string())
            .collect::<Vec<_>>();
        result.sort();
        return result;
    }

    fn find_z_location(&self, a_start: &str) -> ZLocation {
        let step_length = self.steps.len() as i32;
        let mut index = 0;
        let mut step_count = 0;
        let mut current_step = a_start.to_string();
        loop {
            if current_step.ends_with("Z") {
                return ZLocation {
                    step_index: step_count,
                    node: current_step.to_string(),
                };
            }

            let node = self.adjacency_map.get(&current_step).unwrap();
            let next_step = self.steps[index as usize];
            current_step = if next_step == 'R' {
                node.right.to_string()
            } else {
                node.left.to_string()
            };

            index += 1;
            if index == step_length {
                index = 0;
            }
            step_count += 1;
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct ZLocation {
    step_index: i32,
    node: String,
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
