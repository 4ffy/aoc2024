use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

#[derive(Debug)]
enum Operator {
    And,
    Or,
    Xor,
}

#[derive(Debug)]
enum Expr {
    Declare {
        name: String,
        value: bool,
    },
    Binary {
        operator: Operator,
        left: String,
        right: String,
        dest: String,
    },
}

fn declare_line(line: &str) -> bool {
    line.find(':').is_some()
}

fn binary_line(line: &str) -> bool {
    line.find("->").is_some()
}

fn empty_line(line: &str) -> bool {
    line.is_empty()
}

fn parse(src: &str) -> Vec<Expr> {
    let mut result = Vec::new();
    let mut words = Vec::new();
    for line in src.lines() {
        words.clear();
        if declare_line(line) {
            words.extend(line.split_ascii_whitespace());
            if words.len() != 2 {
                panic!("Bad input.");
            }
            let name = (&words[0][..words[0].len() - 1]).to_owned();
            let value = match words[1] {
                "0" => false,
                "1" => true,
                _ => panic!("Bad input"),
            };
            result.push(Expr::Declare { name, value });
        } else if binary_line(line) {
            words.extend(line.split_ascii_whitespace());
            if words.len() != 5 {
                panic!("Bad input.");
            }
            let left = words[0].to_owned();
            let right = words[2].to_owned();
            let dest = words[4].to_owned();
            let operator = match words[1] {
                "AND" => Operator::And,
                "OR" => Operator::Or,
                "XOR" => Operator::Xor,
                _ => panic!("Bad input."),
            };
            result.push(Expr::Binary {
                operator,
                left,
                right,
                dest,
            })
        } else if empty_line(line) {
            continue;
        } else {
            panic!("Bad input.");
        }
    }
    result
}

fn execute(stmts: &[Expr]) -> HashMap<String, bool> {
    let mut env: HashMap<String, bool> = HashMap::new();
    let mut visited: Vec<bool> = vec![false; stmts.len()];
    let mut done = false;
    while !done {
        done = true;
        for (i, stmt) in stmts.iter().enumerate() {
            if !visited[i] {
                match stmt {
                    Expr::Declare { name, value } => {
                        env.insert(name.clone(), *value);
                        visited[i] = true;
                        done = false;
                    }
                    Expr::Binary {
                        operator,
                        left,
                        right,
                        dest,
                    } => {
                        if env.contains_key(left) && env.contains_key(right) {
                            let value = match operator {
                                Operator::And => {
                                    *env.get(left).unwrap()
                                        && *env.get(right).unwrap()
                                }
                                Operator::Or => {
                                    *env.get(left).unwrap()
                                        || *env.get(right).unwrap()
                                }
                                Operator::Xor => {
                                    *env.get(left).unwrap()
                                        != *env.get(right).unwrap()
                                }
                            };
                            env.insert(dest.clone(), value);
                            visited[i] = true;
                            done = false;
                        }
                    }
                }
            }
        }
    }
    env
}

fn z(env: &HashMap<String, bool>) -> i64 {
    let mut z = Vec::new();
    for (k, v) in env.iter() {
        if k.starts_with('z') {
            z.push((k.clone(), v));
        }
    }
    z.sort_by_key(|x| x.0.clone());
    z.reverse();
    let mut result = 0;
    for x in z {
        result *= 2;
        if *x.1 {
            result += 1
        }
    }
    result
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} input", args[0]);
        process::exit(1);
    }
    let data = fs::read_to_string(&args[1]).unwrap();
    let stmts = parse(&data);
    let env = execute(&stmts);
    println!("{}", z(&env));
}
