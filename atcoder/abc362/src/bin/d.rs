use proconio::input;
use std::collections::BinaryHeap;

fn main() {
    input! {
        n: usize,
        m: usize,
        a: [i64; n],
        uvb: [(usize, usize, i64); m],
    }
    let mut g = vec![vec![]; n];
    for &(mut u, mut v, b) in &uvb {
        u -= 1;
        v -= 1;
        g[u].push((v, b));
        g[v].push((u, b));
    }
    let mut dist = vec![i64::MAX; n];
    let mut h = BinaryHeap::new();
    h.push((-a[0], 0));
    while !h.is_empty() {
        let (negc, u) = h.pop().unwrap();
        let c = -negc;
        if dist[u] < c {
            continue;
        }
        dist[u] = c;
        for &(v, b) in &g[u] {
            let nc = c + b + a[v];
            if nc < dist[v] {
                h.push((-nc, v));
            }
        }
    }
    for i in 1..n {
        print!("{} ", dist[i]);
    }
    println!();
}
