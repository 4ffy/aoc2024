use std::env;
use std::error::Error;
use std::fs;
use std::process;
use std::str::FromStr;

#[derive(Debug)]
struct Report(Vec<i32>);

impl Report {
    pub fn new(v: Vec<i32>) -> Self {
        Self(v)
    }

    fn is_increasing(&self) -> bool {
        for i in 0..(self.0.len() - 1) {
            if self.0[i + 1] <= self.0[i] {
                return false;
            }
        }
        true
    }

    fn is_decreasing(&self) -> bool {
        for i in 0..(self.0.len() - 1) {
            if self.0[i + 1] >= self.0[i] {
                return false;
            }
        }
        true
    }

    fn is_smooth(&self) -> bool {
        for i in 0..(self.0.len() - 1) {
            if (self.0[i + 1] - self.0[i]).abs() > 3 {
                return false;
            }
        }
        true
    }

    pub fn is_safe_naive(&self) -> bool {
        (self.is_increasing() || self.is_decreasing()) && self.is_smooth()
    }

    pub fn is_safe(&self) -> bool {
        if (self.is_increasing() || self.is_decreasing()) && self.is_smooth() {
            true
        } else {
            for i in 0..self.0.len() {
                let mut v = self.0.clone();
                v.remove(i);
                let sub = Report::new(v);
                if sub.is_safe_naive() {
                    return true;
                }
            }
            false
        }
    }
}

impl FromStr for Report {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let result = s
            .split_whitespace()
            .map(|x| i32::from_str(x))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Report(result))
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} input", args[0]);
        process::exit(1);
    }

    let data = fs::read_to_string(&args[1])?;

    let mut sum = 0;
    for line in data.lines() {
        let report = Report::from_str(&line)?;
        if report.is_safe() {
            sum += 1;
        }
    }

    println!("{sum}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_the_thing() {
        let data = vec![
            ("7 6 4 2 1", true),
            ("1 2 7 8 9", false),
            ("9 7 6 2 1", false),
            ("1 3 2 4 5", true),
            ("8 6 4 4 1", true),
            ("1 3 6 7 9", true),
        ];
        for (input, expected) in data {
            dbg!(&input);
            assert_eq!(Report::from_str(input).unwrap().is_safe(), expected)
        }
    }
}
