use std::collections::BTreeSet;

fn main() {
    proconio::input! {
        n: usize,
        m: i64,
        mut abc: [(usize, usize, i64); m],
    }

    // adjust to 0-idx
    for (a, b, _) in &mut abc {
        *a -= 1;
        *b -= 1;
    }

    // make graph
    let mut g = vec![vec![]; n];
    for &(a, b, c) in &abc {
        g[b].push((a, c));
        g[a].push((b, -c));
    }

    // find connected components
    let mut used = vec![false; n];
    let mut con = vec![];
    fn dfs(u: usize, val: i64, g: &Vec<Vec<(usize, i64)>>, s: &mut BTreeSet<(i64, usize)>, used: &mut Vec<bool>) {
        used[u] = true;
        s.insert((val, u));
        for &(v, c) in &g[u] {
            if used[v] {
                continue;
            }
            dfs(v, val + c, g, s, used);
        }
    }
    for u in 0..n {
        if !used[u] {
            let mut s = BTreeSet::new();
            dfs(u, 0, &g, &mut s, &mut used);
            con.push(s);
        }
    }

    // calculate bitset for each connected components
    let mut ids = vec![];
    let mut offs = vec![];
    let mut ts = vec![];
    for s in &con {
        let mut id = vec![];
        let mut t = 0;
        let mut off = vec![];
        let base = s.first().unwrap().0;
        for &(rel, u) in s {
            let k = rel - base;
            id.push(u);
            off.push(k as usize);
            t = t | 1<<k;
        }
        ids.push(id);
        offs.push(off);
        ts.push(t);
    }

    // enumerate all bitset for each popcount
    let mut bits = vec![vec![]; n+1];
    for t in 0usize..1<<n {
        bits[t.count_ones() as usize].push(t);
    }

    // bit dp
    let mut ans = vec![-1; n];
    let l = ts.len();
    // check whether the rank of i th component is uniquely determined
    for i in 0..l {
        let mut cnt = 0;
        let mut dp = vec![false; 1<<n];
        dp[0] = true;
        // look connected components in order (except itself)
        for j in 0..l {
            if j == i {
                continue;
            }
            let t = ts[j];
            let mut ndp = vec![false; 1<<n];
            for &s in &bits[cnt] {
                // minimum rank in s
                for k in 0..n {
                    if t<<k & 1<<n > 0 {
                        break;
                    }
                    ndp[s | t<<k] = ndp[s | t<<k] || dp[s as usize];
                }
            }
            dp = ndp;
            cnt += t.count_ones() as usize;
        }
        let t = ts[i];
        let all = (1<<n) - 1;
        let mut ok = vec![];
        for k in 0..n {
            if t<<k & 1<<n > 0 {
                break;
            }
            if dp[all ^ t<<k] {
                ok.push(k);
            }
        }
        if ok.len() == 1 {
            for (j, &u) in ids[i].iter().enumerate() {
                ans[u] = (1 + ok[0] + offs[i as usize][j as usize]) as i64;
            }
        }
    }

    for u in 0..n {
        print!("{} ", ans[u]);
    }
    println!();
}
