fn main() {
    let input = include_str!("./input.txt");
    let almanac = parse_almanac(&input);

    part1(&almanac);
    part2(&almanac);
}

fn part1(almanac: &Almanac) {
    let seed_locations = almanac.seed_location_numbers();
    let lowest_location = seed_locations.iter().min().unwrap();
    println!("Part 1: {:?}", lowest_location);
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<Seed>,
    seed_pairs: Vec<SeedPair>,
    category_maps: Vec<CategoryMap>,
}

impl Almanac {
    fn seed_location_numbers(&self) -> Vec<i64> {
        return self
            .seeds
            .iter()
            .map(|seed| {
                self.category_maps
                    .iter()
                    .fold(seed.number, |acc, e| e.look_up_destination(acc))
            })
            .collect::<Vec<i64>>();
    }
}

#[derive(Debug)]
struct Seed {
    number: i64,
}

#[derive(Debug)]
struct SeedPair {
    start: i64,
    length: i64,
}

impl SeedPair {
    fn seeds_from_range(&self) -> Vec<Seed> {
        return (self.start..(self.start + self.length))
            .map(|number| Seed { number })
            .collect::<Vec<_>>();
    }
}

#[derive(Debug)]
struct CategoryMapping {
    destination_range_start: i64,
    source_range_start: i64,
    range_length: i64,
}

impl CategoryMapping {
    fn look_up_destination(&self, source_number: i64) -> Option<i64> {
        if source_number >= self.source_range_start
            && source_number <= (self.source_range_start + self.range_length)
        {
            let diff_from_start = source_number - self.source_range_start;
            return Some(self.destination_range_start + diff_from_start);
        }
        return None;
    }
}

#[derive(Debug)]
struct CategoryMap {
    mappings: Vec<CategoryMapping>,
}

impl CategoryMap {
    fn look_up_destination(&self, source_number: i64) -> i64 {
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
            number: num.trim().parse::<i64>().unwrap(),
        })
        .collect::<Vec<Seed>>();

    let windows = raw_seeds.windows(2);
    let mut seed_pairs = Vec::new();
    for (index, window) in windows.enumerate() {
        if index % 2 == 1 {
            continue;
        }
        seed_pairs.push(SeedPair {
            start: window[0].trim().parse::<i64>().unwrap(),
            length: window[1].trim().parse::<i64>().unwrap(),
        });
    }

    let mut category_maps = Vec::new();
    for section in splits[1..].iter() {
        let mappings = section.split("\n").collect::<Vec<_>>()[1..]
            .iter()
            .filter(|row| !row.trim().is_empty())
            .map(|row| row.split(" ").map(|num| num.trim()).collect::<Vec<_>>())
            .map(|row_nums| CategoryMapping {
                destination_range_start: row_nums[0].trim().parse::<i64>().unwrap(),
                source_range_start: row_nums[1].trim().parse::<i64>().unwrap(),
                range_length: row_nums[2].trim().parse::<i64>().unwrap(),
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
