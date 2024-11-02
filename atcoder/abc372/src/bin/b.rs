use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        mut m: usize,
    }
    let mut v = vec![];
    for i in 0..=10 {
        v.push(usize::pow(3, i));
    }
    let mut ans = vec![];
    let mut p = 10;
    while m > 0 {
        if v[p] <= m {
            ans.push(p);
            m -= v[p];
        } else {
            p -= 1;
        }
    }
    println!("{}", ans.len());
    println!("{}", ans.iter().join(" "));
}
