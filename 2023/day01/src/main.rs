fn main() {
    let input = include_str!("./input.txt");
    part1(input);
    part2(input);
}

fn part1(input: &str) {
    let calibration_values = input.split("\n").map(|line| {
        if line.trim().len() == 0 {
            return 0;
        }

        let mut first_number: Option<char> = None;
        let mut last_number: char = '0';
        for c in line.chars() {
            if c.is_numeric() {
                if first_number.is_none() {
                    first_number = Some(c);
                }
                last_number = c;
            }
        }
        return format!("{}{}", first_number.unwrap(), last_number)
            .parse::<i32>()
            .unwrap();
    });
    print!("Part1: {}\n", calibration_values.sum::<i32>())
}

fn part2(input: &str) {
    let calibration_values = input.split("\n").map(|line| {
        if line.trim().len() == 0 {
            return 0;
        }

        let first_number = find_first_number(line);
        let last_number = find_last_number(line);
        return format!("{}{}", first_number, last_number)
            .parse::<i32>()
            .unwrap();
    });

    print!("Part2: {}\n", calibration_values.sum::<i32>())
}

fn find_first_number(input: &str) -> String {
    let mut first_number: Option<char> = None;
    for (i, c) in input.chars().enumerate() {
        if c.is_numeric() {
            first_number = Some(c);
            break;
        }

        let max_length_str = &input[i..(std::cmp::min(i + 5, input.len()))];
        let number_word = starts_with_number_word(max_length_str);
        if number_word.is_some() {
            first_number = Some(number_word.unwrap());
            break;
        }
    }
    return first_number.unwrap().to_string();
}

fn find_last_number(input: &str) -> String {
    let mut last_number: Option<char> = None;
    for (i, c) in input.chars().rev().enumerate() {
        if c.is_numeric() {
            last_number = Some(c);
            break;
        }

        let max_length_str =
            &input[(input.len() - 1 - i)..(std::cmp::min(input.len() - 1 - i + 5, input.len()))];
        let number_word = starts_with_number_word(max_length_str);
        if number_word.is_some() {
            last_number = Some(number_word.unwrap());
            break;
        }
    }
    return last_number.unwrap().to_string();
}

fn starts_with_number_word(input: &str) -> Option<char> {
    if input.starts_with("one") {
        return Some('1');
    } else if input.starts_with("two") {
        return Some('2');
    } else if input.starts_with("three") {
        return Some('3');
    } else if input.starts_with("four") {
        return Some('4');
    } else if input.starts_with("five") {
        return Some('5');
    } else if input.starts_with("six") {
        return Some('6');
    } else if input.starts_with("seven") {
        return Some('7');
    } else if input.starts_with("eight") {
        return Some('8');
    } else if input.starts_with("nine") {
        return Some('9');
    }
    return None;
}
