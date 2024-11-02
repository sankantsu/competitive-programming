use itertools::Itertools;
use proconio::input;
use rand::prelude::*;

fn gen() -> (usize, usize, Vec<usize>, Vec<usize>) {
    let n = 5;
    let k = random::<usize>() % 5;
    let mut x = vec![];
    let mut a = vec![];
    for _ in 0..n {
        x.push(1 + random::<usize>() % n);
    }
    for _ in 0..n {
        a.push(1 + random::<usize>() % 10);
    }
    (n, k, x, a)
}

fn solve(n: usize, k: usize, x: &[usize], a: &[usize]) -> Vec<usize> {
    // 1-idx to 0-idx
    let mut x = x.iter().cloned().collect::<Vec<_>>();
    for i in 0..n {
        x[i] -= 1;
    }

    // decompose functional graph

    // refcnt
    let mut r = vec![0usize; n];
    for i in 0..n {
        r[x[i]] += 1;
    }

    // refcnt == 0 (start of chain)
    let mut l = vec![];
    for i in 0..n {
        if r[i] == 0 {
            l.push(i);
        }
    }

    // list all chains
    let mut chains = vec![];
    for i in l {
        let mut chain = vec![];
        let mut j = i;
        while r[j] == 0 {
            chain.push(j);
            r[x[j]] -= 1;
            j = x[j];
        }
        chain.push(j);
        chains.push(chain);
    }

    // list all cycles
    let mut cycles = vec![];
    for i in 0..n {
        if r[i] == 0 {
            continue;
        }
        let mut cycle = vec![];
        let mut j = i;
        while x[j] != i {
            cycle.push(j);
            j = x[j];
        }
        cycle.push(j);
        cycles.push(cycle);
    }

    // each vertex position in chain/cycle
    // (is_cycle, outer_index , inner_index)
    let mut cycle_or_chain_idx = vec![(false, n, n); n];
    for (outer_idx, cycle) in cycles.iter().enumerate() {
        for (inner_idx, i) in cycle.iter().enumerate() {
            cycle_or_chain_idx[*i] = (true, outer_idx, inner_idx);
        }
    }
    for (outer_idx, chain) in chains.iter().enumerate() {
        for (inner_idx, i) in chain.iter().enumerate() {
            if inner_idx != chain.len() - 1 {
                cycle_or_chain_idx[*i] = (false, outer_idx, inner_idx);
            }
        }
    }

    let mut indices = vec![n; n];
    for i in 0..n {
        let mut j = i;
        let mut d = 0;
        loop {
            let (is_cycle, outer_idx, inner_idx) = cycle_or_chain_idx[j];
            if !is_cycle {
                let chain = &chains[outer_idx];
                let l = chain.len();
                if inner_idx + (k - d) <= l - 1 {
                    indices[i] = chain[inner_idx + (k - d)];
                } else {
                    d += l - 1 - inner_idx;
                    j = chain[l - 1];
                }
            } else {
                let cycle = &cycles[outer_idx];
                let l = cycle.len();
                indices[i] = cycle[(inner_idx + (k - d)) % l];
            }
            if indices[i] != n {
                break;
            }
        }
    }

    let mut b = vec![0; n];
    for i in 0..n {
        b[i] = a[indices[i]];
    }
    b
}

fn solve_jury(n: usize, k: usize, x: &[usize], a: &[usize]) -> Vec<usize> {
    // 1-idx to 0-idx
    let mut x = x.iter().cloned().collect::<Vec<_>>();
    for i in 0..n {
        x[i] -= 1;
    }

    let mut d = vec![vec![usize::MAX; n]; 61];
    for v in 0..n {
        d[0][v] = x[v];
    }
    for i in 0..60 {
        for v in 0..n {
            d[i + 1][v] = d[i][d[i][v]];
        }
    }
    let mut ans = vec![0; n];
    for u in 0..n {
        let mut v = u;
        for i in 0..61 {
            if (k >> i) & 1 == 1 {
                v = d[i][v];
            }
        }
        ans[u] = a[v];
    }
    ans
}

fn test() {
    let n_test = 1000;
    for _ in 0..n_test {
        let (n, k, x, a) = gen();
        let expected = solve_jury(n, k, &x, &a);
        let actual = solve(n, k, &x, &a);
        for i in 0..n {
            if expected[i] != actual[i] {
                eprintln!("Expected {}", expected.iter().join(" "));
                eprintln!("Actual {}", actual.iter().join(" "));
                println!("{} {}", n, k);
                println!("{}", x.iter().join(" "));
                println!("{}", a.iter().join(" "));
                std::process::exit(1);
            }
        }
    }
}

fn main() {
    // test();

    input! {
        n: usize,
        k: usize,
        x: [usize; n],
        a: [usize; n],
    }

    let b = solve(n, k, &x, &a);
    println!("{}", b.iter().join(" "));
}
