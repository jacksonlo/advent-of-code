fn main() {
    let input = include_str!("./input.txt");
    part1(input);
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
        return format!("{}{}", first_number.unwrap(), last_number).parse::<i32>().unwrap();
    });
    print!("{}", calibration_values.sum::<i32>())
}
