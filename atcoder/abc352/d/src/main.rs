use std::cmp::min;
use std::collections::BTreeSet;

fn main() {
    proconio::input! {
        n: usize,
        k: usize,
        p: [usize; n],
    }

    let mut idx = vec![0; n+1];
    for (i, &v) in p.iter().enumerate() {
        idx[v] = i;
    }

    let mut bs = BTreeSet::new();
    for v in 1..=k {
        bs.insert(idx[v]);
    }
    let mut ans = bs.last().unwrap() - bs.first().unwrap();

    for v in k+1..=n {
        bs.remove(&idx[v-k]);
        bs.insert(idx[v]);
        ans = min(ans, bs.last().unwrap() - bs.first().unwrap());
    }
    println!("{}", ans);
}
