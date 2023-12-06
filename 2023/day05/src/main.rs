use std::sync::{Arc, Mutex};
use std::thread;
use std::time::SystemTime;

fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let almanac: Almanac = parse_almanac(&input);
    let seed_locations = almanac.seed_location_numbers();
    let lowest_location = seed_locations.iter().min().unwrap();
    println!("Part 1: {:?}", lowest_location);
}

fn part2(input: &str) {
    // Brute force, cpu~
    let now = SystemTime::now();
    println!("Start Part 2: {:?}", now);

    let almanac: Almanac = parse_almanac(&input);
    let lowest_locations = Arc::new(Mutex::new(Vec::new()));
    let category_maps = almanac.category_maps;

    for seed_pair in &almanac.seed_pairs {
        let seeds = seed_pair.seeds_from_range();
        // println!("seeds: {:?}", seeds);
        let seed_chunks = seeds.chunks(10000000 / 5);

        thread::scope(|scope| {
            println!("Starting scope for {:?}", seed_pair);

            seed_chunks.for_each(|chunk| {
                scope.spawn(|| {
                    let seed_locations = seed_location_numbers(&chunk.to_vec(), &category_maps);
                    let seed_pair_lowest_location = seed_locations.iter().min().unwrap();
                    lowest_locations
                        .lock()
                        .unwrap()
                        .push(*seed_pair_lowest_location);
                });
            });

            println!(
                "Scope finished for {:?}, elapsed: {:?}",
                seed_pair,
                now.elapsed().unwrap().as_secs() as f64 / 60.0
            );
        });
    }

    let lowest_location = *lowest_locations.lock().unwrap().iter().min().unwrap();
    println!("Part 2: {:?}", lowest_location);
    println!(
        "Minutes Elapsed: {:?}",
        now.elapsed().unwrap().as_secs() as f64 / 60.0
    );
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<Seed>,
    seed_pairs: Vec<SeedPair>,
    category_maps: Vec<CategoryMap>,
}

impl Almanac {
    fn seed_location_numbers(&self) -> Vec<u64> {
        return seed_location_numbers(&self.seeds, &self.category_maps);
    }
}

fn seed_location_numbers(seeds: &Vec<Seed>, category_maps: &Vec<CategoryMap>) -> Vec<u64> {
    return seeds
        .iter()
        .map(|seed| {
            category_maps
                .iter()
                .fold(seed.number, |acc, e| e.look_up_destination(acc))
        })
        .collect::<Vec<u64>>();
}

#[derive(Debug, Copy, Clone)]
struct Seed {
    number: u64,
}

#[derive(Debug)]
struct SeedPair {
    start: u64,
    length: u64,
}

impl SeedPair {
    fn seeds_from_range(&self) -> Vec<Seed> {
        return (self.start..(self.start + self.length))
            .map(|number| Seed { number })
            .collect::<Vec<_>>();
    }
}

#[derive(Debug, Clone)]
struct CategoryMap {
    mappings: Vec<CategoryMapping>,
}

impl CategoryMap {
    fn look_up_destination(&self, source_number: u64) -> u64 {
        let mut destination = source_number;
        for map in &self.mappings {
            if let Some(d) = map.look_up_destination(source_number) {
                destination = d;
                break;
            }
        }
        return destination;
    }
}

#[derive(Debug, Copy, Clone)]
struct CategoryMapping {
    destination_range_start: u64,
    source_range_start: u64,
    range_length: u64,
}

impl CategoryMapping {
    fn look_up_destination(&self, source_number: u64) -> Option<u64> {
        if source_number >= self.source_range_start
            && source_number < (self.source_range_start + self.range_length)
        {
            let diff_from_start = source_number - self.source_range_start;
            return Some(self.destination_range_start + diff_from_start);
        }
        return None;
    }
}

fn parse_almanac(input: &str) -> Almanac {
    let splits = input.split("\n\n").collect::<Vec<_>>();
    let raw_seeds = splits[0].split(":").collect::<Vec<_>>()[1]
        .split(" ")
        .map(|num| num.trim())
        .filter(|num| !num.is_empty())
        .collect::<Vec<_>>();
    let seeds = raw_seeds
        .iter()
        .map(|num| Seed {
            number: num.trim().parse::<u64>().unwrap(),
        })
        .collect::<Vec<Seed>>();

    let windows = raw_seeds.windows(2);
    let mut seed_pairs = Vec::new();
    for (index, window) in windows.enumerate() {
        if index % 2 == 1 {
            continue;
        }
        seed_pairs.push(SeedPair {
            start: window[0].trim().parse::<u64>().unwrap(),
            length: window[1].trim().parse::<u64>().unwrap(),
        });
    }

    let mut category_maps = Vec::new();
    for section in splits[1..].iter() {
        let mappings = section.split("\n").collect::<Vec<_>>()[1..]
            .iter()
            .filter(|row| !row.trim().is_empty())
            .map(|row| row.split(" ").map(|num| num.trim()).collect::<Vec<_>>())
            .map(|row_nums| CategoryMapping {
                destination_range_start: row_nums[0].trim().parse::<u64>().unwrap(),
                source_range_start: row_nums[1].trim().parse::<u64>().unwrap(),
                range_length: row_nums[2].trim().parse::<u64>().unwrap(),
            })
            .collect::<Vec<CategoryMapping>>();
        category_maps.push(CategoryMap { mappings });
    }

    return Almanac {
        seeds,
        seed_pairs,
        category_maps,
    };
}
