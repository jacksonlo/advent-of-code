use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    // part2(&input);
}

fn part1(input: &str) {
    let tile_map = parse_tile_map(input);

    // Default round down because its integer division
    let steps_to_farthest_point = tile_map.tiles_in_loop() / 2;
    println!("Part 1: {}", steps_to_farthest_point);
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct Position {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct TileMap {
    map: HashMap<Position, Tile>,
}

impl TileMap {
    fn get_starting_position(&self) -> Position {
        for (position, tile) in self.map.iter() {
            if tile.pipe_char == 'S' {
                return *position;
            }
        }
        panic!("No starting position found");
    }

    fn tiles_in_loop(&self) -> i32 {
        let starting_position = self.get_starting_position();
        // DFS to find a path to loop to itself
        let loop_positions = self.traverse(&starting_position, &starting_position);
        return loop_positions.len() as i32;
    }

    fn traverse(&self, start_position: &Position, ending_position: &Position) -> Vec<Position> {
        // TODO: trace the path to the ending position, that is the result
        let mut queue = VecDeque::from(vec![(*start_position, vec![])]);
        let mut visited = HashSet::new();
        while !queue.is_empty() {
            let (current_position, path) = queue.pop_back().unwrap();
            if current_position == *ending_position && visited.len() > 0 {
                // println!("Found path to self");
                // println!("Path: {:?}", path);
                return path;
            }
            if visited.contains(&current_position) {
                continue;
            }

            // println!("Current position: {:?}", current_position);
            visited.insert(current_position);

            for connection in self.map[&current_position].connections.iter() {
                let mut new_path = path.clone();
                new_path.push(current_position.clone());
                queue.push_back((*connection, new_path));
            }
        }
        return vec![];
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct Tile {
    position: Position,
    pipe_char: char,
    connections: Vec<Position>,
}

fn parse_tile_map(input: &str) -> TileMap {
    let char_map = input
        .split("\n")
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();
    let mut map = HashMap::new();
    for (y, line) in char_map.iter().enumerate() {
        for (x, pipe_char) in line.iter().enumerate() {
            let position = Position {
                x: x as i32,
                y: y as i32,
            };
            let mut connections = Vec::new();
            let possible_positions = get_possible_adjacent_positions(pipe_char);
            for possible_position in possible_positions.iter() {
                let adjacent_position = Position {
                    x: position.x + possible_position.x,
                    y: position.y + possible_position.y,
                };
                let adjacent_pipe_char = char_map
                    .get(adjacent_position.y as usize)
                    .and_then(|line| line.get(adjacent_position.x as usize));
                if adjacent_pipe_char.is_some() {
                    let opposite = get_opposite_direction(possible_position);
                    let adjacent_possible_positions =
                        get_possible_adjacent_positions(adjacent_pipe_char.unwrap());
                    if adjacent_possible_positions.contains(&opposite) {
                        connections.push(adjacent_position);
                    }
                }
            }

            let tile = Tile {
                position,
                pipe_char: *pipe_char,
                connections: connections,
            };
            map.insert(position, tile);
        }
    }
    return TileMap { map };
}

const UP: Position = Position { x: 0, y: -1 };
const DOWN: Position = Position { x: 0, y: 1 };
const LEFT: Position = Position { x: -1, y: 0 };
const RIGHT: Position = Position { x: 1, y: 0 };

fn get_possible_adjacent_positions(pipe_char: &char) -> Vec<Position> {
    if *pipe_char == '|' {
        return vec![UP, DOWN];
    } else if *pipe_char == '-' {
        return vec![LEFT, RIGHT];
    } else if *pipe_char == 'L' {
        return vec![UP, RIGHT];
    } else if *pipe_char == 'J' {
        return vec![UP, LEFT];
    } else if *pipe_char == '7' {
        return vec![DOWN, LEFT];
    } else if *pipe_char == 'F' {
        return vec![DOWN, RIGHT];
    } else if *pipe_char == '.' {
        return vec![];
    } else if *pipe_char == 'S' {
        return vec![UP, DOWN, LEFT, RIGHT];
    }
    panic!("Unknown pipe char: {}", pipe_char);
}

fn get_opposite_direction(direction: &Position) -> Position {
    if *direction == UP {
        return DOWN;
    } else if *direction == DOWN {
        return UP;
    } else if *direction == LEFT {
        return RIGHT;
    } else if *direction == RIGHT {
        return LEFT;
    }
    panic!("Unknown direction: {:?}", direction);
}
