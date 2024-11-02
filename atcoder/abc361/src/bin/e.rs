use proconio::input;

fn dfs(u: usize, p: usize, g: &Vec<Vec<(usize, i64)>>) -> (usize, i64) {
    let mut res = (u, 0);
    for (v, cost) in &g[u] {
        if *v == p {
            continue;
        }
        let (t, d) = dfs(*v, u, g);
        if res.1 < d + cost {
            res = (t, d + cost);
        }
    }
    res
}

fn main() {
    input! {
        n: usize,
        abc: [(usize, usize, i64); n - 1],
    }
    let mut g = vec![vec![]; n];
    let s: i64 = abc.iter().map(|x| x.2).sum();
    for (mut a, mut b, c) in abc {
        a -= 1;
        b -= 1;
        g[a].push((b, c));
        g[b].push((a, c));
    }

    let (v, _) = dfs(0, !0, &g);
    let (_, d) = dfs(v, !0, &g);

    let ans = 2 * s - d;
    println!("{}", ans);
}
