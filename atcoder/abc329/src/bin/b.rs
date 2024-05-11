use proconio::input;
use std::collections::BTreeSet;

fn main() {
    input! {
        n: usize,
        a: [i64; n],
    }
    
    let mut s = BTreeSet::new();
    for x in &a {
        s.insert(*x);
    }

    let mx = *s.last().unwrap();
    s.remove(&mx);

    let ans = *s.last().unwrap();
    println!("{}", ans);
}
