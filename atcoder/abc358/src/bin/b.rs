use std::cmp::max;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: i64,
        t: [i64; n],
    }
    let mut ans = 0i64;
    for t in &t {
        ans = max(ans, *t) + a;
        println!("{ans}");
    }
}
