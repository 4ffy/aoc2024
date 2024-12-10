use std::collections::HashSet;
use std::convert::TryInto;
use std::env;
use std::error::Error;
use std::fs;
use std::ops::AddAssign;
use std::process;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Point {
    y: usize,
    x: usize,
}

#[derive(Clone, Copy, Debug)]
struct WalkResult {
    score: usize,
    rating: usize,
}

impl AddAssign for WalkResult {
    fn add_assign(&mut self, rhs: Self) {
        self.score += rhs.score;
        self.rating += rhs.rating;
    }
}

#[derive(Debug)]
struct Grid {
    data: Vec<i8>,
    height: usize,
    width: usize,
}

impl Grid {
    fn get(&self, y: usize, x: usize) -> i8 {
        self.data[y * self.width + x]
    }

    fn walk(&self, y: usize, x: usize) -> WalkResult {
        let mut rating = 0;
        let mut unique: HashSet<Point> = HashSet::new();
        let mut stack: Vec<Point> = Vec::new();
        stack.push(Point { y, x });
        while !stack.is_empty() {
            let curr = stack.pop().unwrap();
            let y = curr.y;
            let x = curr.x;
            let value = self.get(y, x);
            let target = value + 1;
            if value == 9 {
                rating += 1;
                unique.insert(curr);
                continue;
            }
            if y > 0 && self.get(y - 1, x) == target {
                stack.push(Point { y: y - 1, x });
            }
            if y < self.height - 1 && self.get(y + 1, x) == target {
                stack.push(Point { y: y + 1, x });
            }
            if x > 0 && self.get(y, x - 1) == target {
                stack.push(Point { y, x: x - 1 });
            }
            if x < self.width - 1 && self.get(y, x + 1) == target {
                stack.push(Point { y, x: x + 1 });
            }
        }
        WalkResult {
            score: unique.len(),
            rating,
        }
    }

    pub fn score(&self) -> WalkResult {
        let mut result = WalkResult{score: 0, rating: 0};
        for y in 0..self.height {
            for x in 0..self.width {
                if self.get(y, x) == 0 {
                    result += self.walk(y, x);
                }
            }
        }
        result
    }
}

impl FromStr for Grid {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<&str> = s.trim().lines().collect();
        let mut data: Vec<i8> = Vec::new();
        let height = lines.len();
        let width = lines[0].len();
        for line in lines {
            for char in line.chars() {
                if char < '0' && char > '9' {
                    return Err("Invalid format.");
                }
                data.push(char.to_digit(10).unwrap().try_into().unwrap());
            }
        }
        Ok(Self {
            data,
            height,
            width,
        })
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} input", args[0]);
        process::exit(1);
    }
    let data = fs::read_to_string(&args[1])?;
    let grid = Grid::from_str(&data)?;
    let result = grid.score();
    println!("Sum of score: {}", result.score);
    println!("Sum of rating: {}", result.rating);
    Ok(())
}
