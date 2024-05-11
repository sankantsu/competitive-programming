use std::collections::HashSet;
use proconio::input;

fn main() {
    input! {
        n: usize,
        q: usize,
        c: [i64; n],
        mut ab: [(usize, usize); q],
    }
    ab = ab.iter().map(|(a,b)| (a-1, b-1)).collect::<Vec<_>>();

    let mut vs = vec![];
    for &c in &c {
        let mut s = HashSet::new();
        s.insert(c);
        vs.push(s);
    }

    for &(a, b) in &ab {
        let ans;
        if vs[a].len() > vs[b].len() {
            vs.swap(a, b);
        }
        let mut tmp = vs[a].drain().collect::<Vec<_>>();
        for c in tmp.drain(0..) {
            vs[b].insert(c);
        }
        ans = vs[b].len();
        println!("{ans}");
    }
}
