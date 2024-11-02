use proconio::input;
use std::collections::BTreeSet;

#[derive(Debug)]
struct UnionFind {
    par: Vec<usize>,
    rank: Vec<usize>,
    groups: Vec<BTreeSet<usize>>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        let mut par = vec![0; n];
        let rank = vec![0; n];
        let mut groups = vec![BTreeSet::new(); n];
        for i in 0..n {
            par[i] = i;
            groups[i].insert(i);
        }
        Self { par, rank, groups }
    }
    fn find(&mut self, x: usize) -> usize {
        if self.par[x] == x {
            return x;
        }
        self.par[x] = self.find(self.par[x]);
        self.par[x]
    }
    fn unite(&mut self, x: usize, y: usize) {
        let x = self.find(x);
        let y = self.find(y);
        if x == y {
            return;
        }
        let mut merged = self.groups[x]
            .union(&self.groups[y])
            .cloned()
            .collect::<BTreeSet<_>>();
        while merged.len() > 10 {
            merged.pop_first();
        }
        if self.rank[x] < self.rank[y] {
            self.par[x] = y;
            self.groups[y] = merged;
        } else {
            self.par[y] = x;
            self.groups[x] = merged;
            if self.rank[x] == self.rank[y] {
                self.rank[x] += 1;
            }
        }
    }
    fn k_th(&mut self, x: usize, k: usize) -> Option<usize> {
        let r = self.find(x);
        let g = &self.groups[r];
        g.iter().rev().nth(k).cloned()
    }
    #[allow(unused)]
    fn same(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
    }
}

fn main() {
    input! {
        n: usize,
        q: usize,
        queries: [(usize, usize, usize); q],
    }
    let mut uf = UnionFind::new(n);
    for (t, x, y) in queries {
        if t == 1 {
            let u = x - 1;
            let v = y - 1;
            uf.unite(u, v);
        } else if t == 2 {
            let v = x - 1;
            let k = y - 1;
            let ans = uf.k_th(v, k).map_or(-1, |u| (u + 1) as i64);
            println!("{}", ans);
        }
    }
}
