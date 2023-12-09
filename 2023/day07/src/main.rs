use std::collections::HashMap;

use counter::Counter;

fn main() {
    let input = include_str!("./input.txt");
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let cards_map = HashMap::from([
        ('A', 14),
        ('K', 13),
        ('Q', 12),
        ('J', 11),
        ('T', 10),
        ('9', 9),
        ('8', 8),
        ('7', 7),
        ('6', 6),
        ('5', 5),
        ('4', 4),
        ('3', 3),
        ('2', 2),
    ]);

    let mut hands = parse_hands(input);
    hands.sort_by(|a, b| {
        let a_type = a.get_type();
        let b_type = b.get_type();
        if a_type == b_type {
            let a_cards = a
                .cards
                .chars()
                .map(|c| cards_map.get(&c).unwrap())
                .collect::<Vec<_>>();
            let b_cards = b
                .cards
                .chars()
                .map(|c| cards_map.get(&c).unwrap())
                .collect::<Vec<_>>();
            for (x, y) in std::iter::zip(a_cards, b_cards) {
                if x != y {
                    return y.cmp(x);
                }
            }
            panic!("Hands are equal");
        } else {
            return a_type.cmp(&b_type);
        }
    });
    hands.reverse();

    let total_winnings = hands.iter().enumerate().fold(0, |acc, (index, hand)| {
        return acc + hand.bid * (index as u32 + 1);
    });
    println!("Part 1: {:?}", total_winnings);
}

fn part2(input: &str) {
    let cards_map = HashMap::from([
        ('A', 14),
        ('K', 13),
        ('Q', 12),
        ('J', 1),
        ('T', 10),
        ('9', 9),
        ('8', 8),
        ('7', 7),
        ('6', 6),
        ('5', 5),
        ('4', 4),
        ('3', 3),
        ('2', 2),
    ]);

    let mut hands = parse_hands(input);
    hands.sort_by(|a, b| {
        let a_type = a.get_type_with_joker();
        let b_type = b.get_type_with_joker();
        if a_type == b_type {
            let a_cards = a
                .cards
                .chars()
                .map(|c| cards_map.get(&c).unwrap())
                .collect::<Vec<_>>();
            let b_cards = b
                .cards
                .chars()
                .map(|c| cards_map.get(&c).unwrap())
                .collect::<Vec<_>>();
            for (x, y) in std::iter::zip(a_cards, b_cards) {
                if x != y {
                    return y.cmp(x);
                }
            }
            panic!("Hands are equal: {:?} {:?}", a, b);
        } else {
            return a_type.cmp(&b_type);
        }
    });
    hands.reverse();

    let total_winnings = hands.iter().enumerate().fold(0, |acc, (index, hand)| {
        return acc + hand.bid * (index as u32 + 1);
    });
    println!("Part 1: {:?}", total_winnings);
}

#[derive(Debug)]
struct Hand {
    cards: String,
    bid: u32,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

impl Hand {
    fn get_type(&self) -> HandType {
        let char_counts: Counter<char> = self.cards.chars().collect::<Counter<_>>();
        let most_common = char_counts.most_common_ordered();
        if most_common[0].1 == 5 {
            return HandType::FiveOfAKind;
        } else if most_common[0].1 == 4 {
            return HandType::FourOfAKind;
        } else if most_common[0].1 == 3 && most_common[1].1 == 2 {
            return HandType::FullHouse;
        } else if most_common[0].1 == 3 {
            return HandType::ThreeOfAKind;
        } else if most_common[0].1 == 2 && most_common[1].1 == 2 {
            return HandType::TwoPair;
        } else if most_common[0].1 == 2 {
            return HandType::OnePair;
        } else {
            return HandType::HighCard;
        }
    }

    // Return HandType and the char Joker turned into
    fn get_type_with_joker(&self) -> HandType {
        if !self.cards.contains('J') {
            return self.get_type();
        }
        if self.cards == "JJJJJ" {
            return HandType::FiveOfAKind;
        }

        let char_counts: Counter<char> = self.cards.chars().collect::<Counter<_>>();
        let most_common = char_counts.most_common_ordered();
        let joker = most_common
            .iter()
            .filter(|(c, _)| *c != 'J')
            .collect::<Vec<_>>()[0]
            .0;

        return Hand {
            cards: self.cards.replace("J", &joker.to_string()),
            bid: self.bid,
        }
        .get_type();
    }
}

fn parse_hands(input: &str) -> Vec<Hand> {
    return input
        .lines()
        .map(|line| {
            let split = line.split(" ").collect::<Vec<_>>();
            let cards = split[0].trim().to_string();
            let bid = split[1].parse::<u32>().unwrap();
            Hand { bid, cards }
        })
        .collect::<Vec<_>>();
}
