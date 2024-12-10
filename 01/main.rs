use std::env;
use std::fs;
use std::process;
use std::str::FromStr;

fn count(slice: &[i32], elem: i32) -> i32 {
    slice.iter().filter(|x| **x == elem).count() as i32
}

fn distance(left: &[i32], right: &[i32]) -> i32 {
    left.iter()
        .zip(right.iter())
        .fold(0, |a, x| a + (x.1 - x.0).abs())
}

fn similarity(left: &[i32], right: &[i32]) -> i32 {
    left.iter()
        .fold(0, |a, x| a + x * count(right, *x))
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} input", args[0]);
        process::exit(1);
    }

    let data = fs::read_to_string(&args[1])?;

    let mut left: Vec<i32> = data
        .split_whitespace()
        .enumerate()
        .filter_map(|x| if x.0 & 0x1 == 0 { Some(x.1) } else { None })
        .filter_map(|x| i32::from_str(x).ok())
        .collect();

    let mut right: Vec<i32> = data
        .split_whitespace()
        .enumerate()
        .filter_map(|x| if x.0 & 0x1 == 1 { Some(x.1) } else { None })
        .filter_map(|x| i32::from_str(x).ok())
        .collect();

    left.sort();
    right.sort();

    println!("Distance: {}", distance(&left, &right));
    println!("Similarity: {}", similarity(&left, &right));
    Ok(())
}
