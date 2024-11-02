use proconio::{marker::*, *};

fn main() {
    input! { n: usize, s: [Chars; n] }
    let m = s.iter().map(|t| t.len()).max().unwrap();
    for j in 0..m {
        let t: String = s.iter().rev().map(|s| s.get(j).unwrap_or(&'*')).collect();
        println!("{}", t.trim_end_matches('*'));
    }
}
