use proconio::input;
use proconio::marker::Chars;

fn main() {
    input! {
        n: usize,
        t: i64,
        s: Chars,
        mut x: [i64; n],
    }
    for i in 0..n {
        x[i] *= 10;
    }

    let mut v = vec![];
    for i in 0..n {
        if s[i] == '0' {
            v.push(x[i]);
        }
    }
    v.sort();

    let mut ans = 0;
    for i in 0..n {
        if s[i] == '1' {
            let a = x[i];
            let b = x[i] + 2 * (10 * t + 1);
            let l = v.binary_search(&a).unwrap_or_else(|x| x);
            let r = v.binary_search(&b).unwrap_or_else(|x| x);
            ans += r - l;
        }
    }
    println!("{ans}");
}
