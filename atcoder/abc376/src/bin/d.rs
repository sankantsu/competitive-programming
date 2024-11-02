use proconio::input;
use std::collections::VecDeque;

fn main() {
    input! {
        n: usize,
        m: usize,
        ab: [(usize, usize); m],
    }
    let ab = ab.iter().map(|(a, b)| (a - 1, b - 1)).collect::<Vec<_>>();
    let mut g = vec![vec![]; n];
    for (a, b) in ab {
        g[a].push(b);
    }
    let mut dist = vec![usize::MAX; n];
    let mut queue = VecDeque::new();
    dist[0] = 0;
    queue.push_back(0);

    let mut ans = usize::MAX;
    while !queue.is_empty() {
        let u = queue.pop_front().unwrap();
        for v in &g[u] {
            if *v == 0 {
                ans = usize::min(ans, dist[u] + 1);
            } else if dist[*v] == usize::MAX {
                dist[*v] = dist[u] + 1;
                queue.push_back(*v);
            }
        }
    }
    println!("{}", if ans == usize::MAX { -1 } else { ans as i64 });
}
