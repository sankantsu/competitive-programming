use proconio::input;
use proconio::marker::Chars;

fn main() {
    input! {
        s: Chars, t: String,
    }
    let mut ans = false;
    for w in 1..=(s.len() - 1) {
        let m = (s.len() + w - 1) / w; // 部分文字列の数
        let v = (0..m)
            .map(|i| &s[w * i..usize::min(s.len(), w * (i + 1))])
            .collect::<Vec<_>>();
        for c in 1..=w {
            let mut u = String::new();
            for ss in &v {
                if ss.len() >= c {
                    u.push(ss[c - 1]);
                }
            }
            if t == u {
                ans = true;
            }
        }
    }
    println!("{}", if ans { "Yes" } else { "No" });
}
