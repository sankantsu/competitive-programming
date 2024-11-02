use proconio::input;
use proconio::marker::Chars;

fn main() {
    input! {
        n: usize,
        q: usize,
        mut s: Chars,
        xc: [(usize, char); q],
    }
    let mut ans = 0;
    for i in 0..=(n - 3) {
        if s[i] == 'A' && s[i + 1] == 'B' && s[i + 2] == 'C' {
            ans += 1;
        }
    }
    for (x, c) in xc {
        let x = x - 1;
        if x + 2 < n && s[x] == 'A' && s[x + 1] == 'B' && s[x + 2] == 'C' {
            ans -= 1;
        } else if x >= 1 && x + 1 < n && s[x - 1] == 'A' && s[x] == 'B' && s[x + 1] == 'C' {
            ans -= 1;
        } else if x >= 2 && s[x - 2] == 'A' && s[x - 1] == 'B' && s[x] == 'C' {
            ans -= 1;
        }
        if x + 2 < n && c == 'A' && s[x + 1] == 'B' && s[x + 2] == 'C' {
            ans += 1;
        } else if x >= 1 && x + 1 < n && s[x - 1] == 'A' && c == 'B' && s[x + 1] == 'C' {
            ans += 1;
        } else if x >= 2 && s[x - 2] == 'A' && s[x - 1] == 'B' && c == 'C' {
            ans += 1;
        }
        s[x] = c;
        println!("{}", ans);
    }
}
