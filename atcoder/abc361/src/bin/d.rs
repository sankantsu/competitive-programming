use proconio::input;
use proconio::marker::Chars;
use std::collections::HashSet;
use std::collections::VecDeque;

fn main() {
    input! {
        n: usize,
        mut s: Chars,
        mut t: Chars,
    }
    for _ in 0..2 {
        s.push('.');
        t.push('.');
    }
    let mut q = VecDeque::new();
    let mut hs = HashSet::new();
    q.push_back((s.clone(), 0));
    hs.insert(s);
    let mut ans = -1;
    while !q.is_empty() {
        let (u, cnt) = q.pop_front().unwrap();
        if u == t {
            ans = cnt;
            break;
        }
        let mut k = 0;
        while u[k] != '.' {
            k += 1;
        }
        for i in 0..=n {
            let mut v = u.clone();
            if u[i] != '.' && u[i + 1] != '.' {
                v[i] = '.';
                v[i + 1] = '.';
                v[k] = u[i];
                v[k + 1] = u[i + 1];
            }
            if !hs.contains(&v) {
                q.push_back((v.clone(), cnt + 1));
                hs.insert(v);
            }
        }
    }
    println!("{}", ans);
}
