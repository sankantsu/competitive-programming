use proconio::input;
use std::cmp::max;
use std::cmp::min;

fn main() {
    input! {
        a: [i64; 6],
        b: [i64; 6],
    }
    let mut ok = true;
    for i in 0..3 {
        let x = max(min(a[i], a[i + 3]), min(b[i], b[i + 3]));
        let y = min(max(a[i], a[i + 3]), max(b[i], b[i + 3]));
        if x >= y {
            ok = false;
        }
    }
    println!("{}", if ok { "Yes" } else { "No" });
}
