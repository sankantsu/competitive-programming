use rand::prelude::*;
use itertools::Itertools;
use proconio::input;
use proconio::marker::Chars;

fn _solve_jury(n: usize, m: usize, s: &Vec<char>, t: &Vec<char>) -> bool {
    let mut ans = false;
    for k in 0..=n-m+1 {  // number of stamps
        let it = (0..=n-m).into_iter().permutations(k);
        for v in it {
            let mut u = vec!['#'; n];
            for &i in &v {
                for j in 0..m {
                    u[i+j] = t[j];
                }
            }
            if *s == u {
                ans = true;
            }
        }
    }
    ans
}

fn solve(n: usize, m: usize, s: &Vec<char>, t: &Vec<char>) -> bool {
    let mut dp = vec![0; n+1];
    dp[0] = 1;

    for i in 0..=n {
        if dp[i] == 1 {
            for j in 0..m {
                if i+j >= n {
                    break;
                }
                if s[i+j] == t[j] {
                    let v = if j == m - 1 { 2 } else { 1 };
                    dp[i+j+1] = dp[i+j+1].max(v);
                } else {
                    break;
                }
            }
        } else if dp[i] == 2 {
            for j in 0..m {
                for k in 0..m {
                    if (k as i64) - (j as i64) < 0 {
                        continue;
                    }
                    if i - j + k >= n {
                        break;
                    }
                    if s[i-j+k] == t[k] {
                        let v = if k == m - 1 { 2 } else { 1 };
                        dp[i-j+k+1] = dp[i-j+k+1].max(v);
                    } else {
                        break;
                    }
                }
            }
        }
    }
    dp[n] == 2
}

fn _gen_testcase() -> (usize, usize, Vec<char>, Vec<char>) {
    let mut rng = thread_rng();
    let n = 5;
    let m = 3;
    let mut random_char = || {
        let c = rng.gen_range(0..3);
        (b'A' + c) as char
    };
    let mut s = vec![];
    let mut t = vec![];
    for _ in 0..n { s.push(random_char()) }
    for _ in 0..m { t.push(random_char()) }
    (n, m, s, t)
}

fn _test() {
    let n_testcase = 1000;
    for _ in 0..n_testcase {
        let (n, m, s, t) = _gen_testcase();
        let ans_jury = if _solve_jury(n, m, &s, &t) { "Yes" } else { "No" };
        let ans = if solve(n, m, &s, &t) { "Yes" } else { "No" };
        if ans != ans_jury {
            let s = s.into_iter().collect::<String>();
            let t = t.into_iter().collect::<String>();
            eprintln!("Wrong answer!");
            eprintln!("Expected: {ans_jury}");
            eprintln!("Actual: {ans}");
            println!("{n} {m}");
            println!("{s}");
            println!("{t}");
            std::process::exit(1);
        }
    }
    eprintln!("Passed all tests!");
}

fn main() {
    _test();

    input! {
        n: usize,
        m: usize,
        s: Chars,
        t: Chars,
    }

    let ans = if solve(n, m, &s, &t) { "Yes" } else { "No" };
    println!("{ans}");
}
