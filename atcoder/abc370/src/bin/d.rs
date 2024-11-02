use proconio::input;
use std::collections::BTreeSet;

fn main() {
    input! {
        h: usize,
        w: usize,
        q: usize,
        rc: [(usize, usize); q],
    }
    let mut rows = vec![];
    let mut cols = vec![];
    for _i in 0..h {
        let mut b = BTreeSet::new();
        for j in 0..w {
            b.insert(j);
        }
        rows.push(b);
    }
    for _j in 0..w {
        let mut b = BTreeSet::new();
        for i in 0..h {
            b.insert(i);
        }
        cols.push(b);
    }

    let mut ans = h * w;
    for (mut r, mut c) in rc {
        r -= 1;
        c -= 1;
        let r = r;
        let c = c;
        if rows[r].contains(&c) {
            ans -= 1;
            rows[r].remove(&c);
            cols[c].remove(&r);
        } else {
            if let Some(x) = rows[r].range(0..c).next_back().cloned() {
                ans -= 1;
                rows[r].remove(&x);
                cols[x].remove(&r);
            }
            if let Some(x) = rows[r].range((c + 1)..w).next().cloned() {
                ans -= 1;
                rows[r].remove(&x);
                cols[x].remove(&r);
            }
            if let Some(x) = cols[c].range(0..r).next_back().cloned() {
                ans -= 1;
                cols[c].remove(&x);
                rows[x].remove(&c);
            }
            if let Some(x) = cols[c].range((r + 1)..h).next().cloned() {
                ans -= 1;
                cols[c].remove(&x);
                rows[x].remove(&c);
            }
        }
    }
    println!("{}", ans);
}
