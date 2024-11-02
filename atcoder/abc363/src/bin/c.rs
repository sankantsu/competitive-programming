use itertools::Itertools;
use proconio::input;
use proconio::marker::Chars;
use std::collections::HashSet;

fn main() {
    input! {
        n: usize,
        k: usize,
        s: Chars,
    }
    let mut str_set = HashSet::new();
    let mut ans = 0;
    for p in (0..n).permutations(n) {
        let mut t = Vec::new();
        for i in p {
            t.push(s[i]);
        }
        if str_set.contains(&t) {
            continue;
        }
        str_set.insert(t.clone());
        // dbg!(&t);
        let mut ok = true;
        for i in 0..=(n - k) {
            let sub = &t[i..i + k];
            let mut is_pal = true;
            for j in 0..k {
                if sub[j] != sub[k - 1 - j] {
                    is_pal = false;
                    break;
                }
            }
            // dbg!(sub, is_pal);
            if is_pal {
                ok = false;
                break;
            }
        }
        if ok {
            ans += 1;
        }
    }
    println!("{}", ans);
}
