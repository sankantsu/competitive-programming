use proconio::input;
use rand::prelude::*;
use std::collections::BTreeSet;

fn solve1(n: usize, a: &[usize], w: &[i64]) -> i64 {
    let mut v = vec![BTreeSet::new(); n];
    for i in 0..n {
        v[a[i]].insert(w[i]);
    }
    let mut ans = 0;
    for i in 0..n {
        ans += w[i];
    }
    for i in 0..n {
        if v[i].is_empty() {
            continue;
        }
        ans -= v[i].last().unwrap();
    }
    ans
}

fn solve2(n: usize, a: &[usize], w: &[i64]) -> i64 {
    let mut v = vec![BTreeSet::new(); n];
    for i in 0..n {
        v[a[i]].insert((w[i], i));
    }
    let mut ans = 0;
    for i in 0..n {
        if v[i].is_empty() {
            continue;
        }
        let s = v[i].iter().map(|p| p.0).sum::<i64>() - v[i].last().unwrap().0;
        ans += s;
    }
    ans
}

fn gen_random() -> (usize, Vec<usize>, Vec<i64>) {
    let mut rng = thread_rng();
    let n = 4;
    let mut a = vec![];
    let mut w = vec![];
    for _ in 0..n {
        a.push(rng.gen_range(0..n));
        w.push(rng.gen_range(1..10));
    }
    (n, a, w)
}

fn test() {
    let mut status = 0;
    for _ in 0..1000 {
        let (n, a, w) = gen_random();
        let ans = solve1(n, &a, &w);
        let ans2 = solve2(n, &a, &w);
        if ans != ans2 {
            eprintln!("Expected: {ans}");
            eprintln!("But got: {ans2}");
            println!("{n}");
            for a in a {
                print!("{a} ");
            }
            println!();
            for w in w {
                print!("{w} ");
            }
            println!();
            status = 1;
            break;
        }
    }
    std::process::exit(status);
}

fn main() {
    input! {
        n: usize,
        mut a: [usize; n],
        w: [i64; n],
    }
    for i in 0..n {
        a[i] -= 1;
    }
    let ans = solve1(n, &a, &w);
    let ans2 = solve2(n, &a, &w);
    assert_eq!(ans, ans2);
    println!("{ans}");
}
