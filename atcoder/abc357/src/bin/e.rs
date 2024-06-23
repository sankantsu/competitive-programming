use proconio::input;
use std::collections::VecDeque;

fn main() {
    input! {
        n: usize,
        mut a: [usize; n],
    }
    a = a.into_iter().map(|x| x - 1).collect();
    // cycle detection
    let mut cnt = vec![0; n];
    a.iter().for_each(|&dest| cnt[dest] += 1);
    let mut q = VecDeque::new();
    for i in 0..n {
        if cnt[i] == 0 {
            q.push_back(i);
        }
    }
    while !q.is_empty() {
        let i = q.pop_front().unwrap();
        cnt[a[i]] -= 1;
        if cnt[a[i]] == 0 {
            q.push_back(a[i]);
        }
    }
    let mut v = vec![0; n];
    for i in 0..n {
        if cnt[i] == 0 { continue; }
        let mut lst = vec![];  // nodes in cycle
        let mut u = i;
        while cnt[u] > 0 {
            cnt[u] -= 1;
            lst.push(u);
            u = a[u];
        }
        lst.iter().for_each(|&u| v[u] = lst.len());
    }
    // count reachable nodes out of cycle
    fn dfs(u: usize, a: &Vec<usize>, memo: &mut Vec<usize>) -> usize {
        if memo[u] > 0 {
            return memo[u];
        }
        let res = 1 + dfs(a[u], a, memo);
        memo[u] = res;
        res
    }
    for u in 0..n {
        v[u] = dfs(u, &a, &mut v);
    }
    let ans: usize = v.iter().sum();
    println!("{ans}");
}
