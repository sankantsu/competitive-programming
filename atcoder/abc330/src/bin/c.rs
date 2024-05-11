use std::cmp::min;
use proconio::input;

fn main() {
    input! {
        d: i64,
    }

    let mut sq = vec![];  // sq[j] = j*j
    let mut p = 0;
    while p*p < d {
        sq.push(p*p);
        p += 1;
    }

    let mut ans = d;
    for i in 0..p {
        let j = sq.binary_search(&(d - i*i)).map_or_else(|v| v as i64, |v| v as i64);
        ans = min(ans, (d - i*i - (j-1)*(j-1)).abs());
        ans = min(ans, (d - i*i - j*j).abs());
    }
    println!("{}", ans);
}
