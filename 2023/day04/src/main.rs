use std::collections::HashSet;

fn main() {
    let input = include_str!("./input.txt");
    part1(input);
    part2(input);
}

fn part1(input: &str) {
    let cards: Vec<Card> = parse_cards(&input);
    let total_worth = cards.iter().map(|c| c.worth()).sum::<i32>();
    println!("Part 1: {}", total_worth);
}

fn part2(input: &str) {
    let cards: Vec<Card> = parse_cards(&input);
    let number_of_cards = count_card_copies(&cards, &cards);
    println!("Part 2: {}", number_of_cards);
}

#[derive(Debug, Clone)]
struct Card {
    index: i32,
    numbers_to_win: HashSet<i32>,
    numbers: HashSet<i32>,
}

impl Card {
    fn winning_numbers(&self) -> HashSet<i32> {
        return self
            .numbers
            .intersection(&self.numbers_to_win)
            .cloned()
            .collect();
    }

    fn worth(&self) -> i32 {
        let count = self.winning_numbers().len();
        if count == 0 {
            return 0;
        }
        return 2_i32.pow((count - 1) as u32);
    }
}

fn parse_cards(input: &str) -> Vec<Card> {
    let cards = input
        .lines()
        .enumerate()
        .map(|(index, line)| {
            let parts = line.split("|").collect::<Vec<_>>();

            let winning_part = parts[0].split(":").collect::<Vec<_>>();
            let numbers_to_win = winning_part[1]
                .trim()
                .split(" ")
                .filter(|s| !s.is_empty())
                .map(|s| s.trim().parse::<i32>().unwrap())
                .collect::<HashSet<_>>();

            let numbers = parts[1]
                .trim()
                .split(" ")
                .filter(|s| !s.is_empty())
                .map(|s| s.trim().parse::<i32>().unwrap())
                .collect::<HashSet<_>>();
            return Card {
                index: (index + 1) as i32,
                numbers_to_win: numbers_to_win,
                numbers: numbers,
            };
        })
        .collect::<Vec<Card>>();
    return cards;
}

fn count_card_copies(current_cards: &Vec<Card>, all_cards: &Vec<Card>) -> i32 {
    let mut count: i32 = 0;
    for card in current_cards.iter() {
        count += 1;
        let winning_count = card.winning_numbers().len();
        if winning_count > 0 {
            if card.index < all_cards.len() as i32 {
                let cards_won = all_cards[card.index as usize
                    ..=std::cmp::min(
                        (card.index - 1) as usize + winning_count as usize,
                        all_cards.len() - 1,
                    )]
                    .to_vec();

                count += count_card_copies(&cards_won, &all_cards);
            }
        }
    }
    return count;
}
