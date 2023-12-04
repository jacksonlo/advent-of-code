use std::collections::HashSet;

fn main() {
    let input = include_str!("./input.txt");
    part1(input);
    part2(input);
}

fn part1(input: &str) {
    let schematic = parse_schematic(input);
    let part_numbers = parse_part_numbers(&schematic);
    let symbols = parse_symbols(&schematic);

    let sum = part_numbers
        .iter()
        .filter(|pn| pn.is_adjacent(&symbols))
        .map(|pn| pn.number)
        .sum::<i32>();
    println!("Part 1: {}", sum);
}

fn part2(input: &str) {
    let schematic = parse_schematic(input);
    let part_numbers = parse_part_numbers(&schematic);
    let symbols = parse_symbols(&schematic);

    let sum = symbols
        .iter()
        .map(|s| s.gear_ratio(&part_numbers))
        .sum::<i32>();
    println!("Part 2: {}", sum);
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct Position {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct PartNumber {
    number: i32,
    start_position: Position,
    end_position: Position,
}

impl PartNumber {
    fn is_adjacent(&self, symbols: &Vec<Symbol>) -> bool {
        let adjacent_positions = self.adjacent_positions();
        let symbol_positions = symbols.iter().map(|s| s.position).collect::<HashSet<_>>();
        return adjacent_positions.intersection(&symbol_positions).count() > 0;
    }

    fn adjacent_positions(&self) -> HashSet<Position> {
        let mut positions = HashSet::new();

        // Can assume its all on same y axis
        let y = self.start_position.y;
        for x in self.start_position.x..=self.end_position.x {
            // Rows above and below
            positions.insert(Position { x: x, y: y - 1 });
            positions.insert(Position { x: x, y: y + 1 });
        }

        // Columns on left
        positions.insert(Position {
            x: self.start_position.x - 1,
            y: y - 1,
        });

        positions.insert(Position {
            x: self.start_position.x - 1,
            y: y,
        });
        positions.insert(Position {
            x: self.start_position.x - 1,
            y: y + 1,
        });

        // Column on right
        positions.insert(Position {
            x: self.end_position.x + 1,
            y: y - 1,
        });

        positions.insert(Position {
            x: self.end_position.x + 1,
            y: y,
        });
        positions.insert(Position {
            x: self.end_position.x + 1,
            y: y + 1,
        });

        // Remove all positions that are outside the schematic
        positions = positions
            .into_iter()
            .filter(|p| p.x >= 0 && p.y >= 0)
            .collect();

        return positions;
    }
}

#[derive(Debug, Copy, Clone)]
struct Symbol {
    symbol: char,
    position: Position,
}

impl Symbol {
    fn gear_ratio(&self, part_numbers: &Vec<PartNumber>) -> i32 {
        let mut gear_ratio = 1;
        let mut count = 0;
        for part_number in part_numbers {
            // Terribly inefficient but already wrote this code from part 1 so it works...
            if part_number.is_adjacent(&vec![*self]) {
                if count == 2 {
                    return 0;
                }
                gear_ratio *= part_number.number;
                count += 1;
            }
        }
        if count != 2 {
            return 0;
        }
        return gear_ratio;
    }
}

fn parse_schematic(input: &str) -> Vec<Vec<char>> {
    let mut schematic = Vec::new();
    for line in input.lines() {
        if line.trim().len() > 0 {
            schematic.push(line.trim().chars().collect());
        }
    }
    return schematic;
}

fn parse_part_numbers(schematic: &Vec<Vec<char>>) -> Vec<PartNumber> {
    let mut part_numbers = Vec::new();
    for (i, row) in schematic.iter().enumerate() {
        let mut current_part = String::new();
        for (j, c) in row.iter().enumerate() {
            if c.is_digit(10) {
                current_part.push(*c);
            } else if !current_part.is_empty() {
                part_numbers.push(PartNumber {
                    number: current_part.parse::<i32>().unwrap(),
                    start_position: Position {
                        x: (j - current_part.len()) as i32,
                        y: i as i32,
                    },
                    end_position: Position {
                        x: (j - 1) as i32,
                        y: i as i32,
                    },
                });
                current_part = String::new();
            }
        }

        // Push last part number at end of row
        if !current_part.is_empty() {
            part_numbers.push(PartNumber {
                number: current_part.parse::<i32>().unwrap(),
                start_position: Position {
                    x: (row.len() - current_part.len()) as i32,
                    y: i as i32,
                },
                end_position: Position {
                    x: (row.len() - 1) as i32,
                    y: i as i32,
                },
            });
        }
    }
    return part_numbers;
}

fn parse_symbols(schematic: &Vec<Vec<char>>) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    for (i, row) in schematic.iter().enumerate() {
        for (j, c) in row.iter().enumerate() {
            if !c.is_alphanumeric() && *c != '.' {
                symbols.push(Symbol {
                    symbol: *c,
                    position: Position {
                        x: j as i32,
                        y: i as i32,
                    },
                });
            }
        }
    }
    return symbols;
}

#[cfg(test)]
mod part_number_tests {

    use super::*;

    #[test]
    fn is_adjacent() {
        let part_number = PartNumber {
            number: 467,
            start_position: Position { x: 0, y: 0 },
            end_position: Position { x: 2, y: 0 },
        };
        let symbols = Vec::from([Symbol {
            symbol: '*',
            position: Position { x: 3, y: 1 },
        }]);
        assert!(part_number.is_adjacent(&symbols));
    }

    #[test]
    fn is_not_adjacent() {
        let part_number = PartNumber {
            number: 467,
            start_position: Position { x: 0, y: 0 },
            end_position: Position { x: 0, y: 2 },
        };
        let symbols = Vec::from([Symbol {
            symbol: '#',
            position: Position { x: 6, y: 4 },
        }]);
        assert!(!part_number.is_adjacent(&symbols));
    }
}
