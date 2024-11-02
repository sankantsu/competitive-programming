use proconio::input;
use proconio::marker::Chars;

fn main() {
    input! {
        mut s: Chars,
        t: Chars,
    }
    let n = s.len();
    let mut v = vec![];
    for i in 0..n {
        if t[i] < s[i] {
            v.push((1, i));
        } else if t[i] > s[i] {
            v.push((2, n - i));
        }
    }
    v.sort();

    let mut ans = vec![];
    for (u, mut i) in v {
        if u == 2 {
            i = n - i;
        }
        s[i] = t[i];
        ans.push(s.iter().collect::<String>());
    }
    println!("{}", ans.len());
    for s in ans {
        println!("{}", s);
    }
}
