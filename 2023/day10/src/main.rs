use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let tile_map = parse_tile_map(input);

    // Default round down because its integer division
    let steps_to_farthest_point = tile_map.get_positions_in_loop().len() / 2;
    println!("Part 1: {}", steps_to_farthest_point);
}

fn part2(input: &str) {
    let tile_map = parse_tile_map(input);
    let loop_positions: HashSet<Position> = HashSet::from_iter(tile_map.get_positions_in_loop());

    let mut tiles_enclosed = 0;
    for (position, tile) in tile_map.map.iter() {
        if loop_positions.contains(position) {
            continue;
        }
        // Draw a line from the position to the left until we hit the edge
        // Over that line, check how many times we hit a position in the loop
        // Odd = enclosed, even = not enclosed
        let line_positions = (0..position.x)
            .map(|x| Position { x, y: position.y })
            .filter(|p| loop_positions.contains(p))
            .filter(|p| tile_map.get(&p).pipe_char != '-')
            .collect::<Vec<Position>>();

        let overlap = line_positions
            .iter()
            .map(|p| tile_map.get(&p))
            .fold(vec![], |acc: Vec<&Tile>, t| {
                // Flatten L J and F 7 to nothing because it doesnt cross
                // Flatten L 7 and F J to 1 because it crosses once
                let mut accc = acc;
                let last = accc.last();
                if last.is_some_and(|l| {
                    (l.pipe_char == 'L' && t.pipe_char == 'J')
                        || (l.pipe_char == 'F' && t.pipe_char == '7')
                }) {
                    accc.pop();
                } else if last.is_some_and(|l| {
                    (l.pipe_char == 'L' && t.pipe_char == '7')
                        || (l.pipe_char == 'F' && t.pipe_char == 'J')
                }) {
                    accc.pop();
                    accc.push(t);
                } else {
                    accc.push(t);
                }
                return accc;
            })
            .len();

        if overlap % 2 == 1 {
            tiles_enclosed += 1;
        }
    }

    println!("Part 2: {}", tiles_enclosed);
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
    fn get(&self, position: &Position) -> &Tile {
        return self.map.get(position).unwrap();
    }

    fn get_starting_position(&self) -> Position {
        for (position, tile) in self.map.iter() {
            if tile.pipe_char == 'S' {
                return *position;
            }
        }
        panic!("No starting position found");
    }

    fn get_positions_in_loop(&self) -> Vec<Position> {
        let starting_position = self.get_starting_position();
        let loop_positions = self.traverse(&starting_position, &starting_position);
        return loop_positions;
    }

    fn traverse(&self, start_position: &Position, ending_position: &Position) -> Vec<Position> {
        let mut queue = VecDeque::from(vec![(*start_position, vec![])]);
        let mut visited = HashSet::new();
        while !queue.is_empty() {
            let (current_position, path) = queue.pop_back().unwrap();
            if current_position == *ending_position && visited.len() > 2 {
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
        panic!("No path found");
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
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
