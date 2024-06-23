use std::collections::*;
use proconio::*;

fn main() {
    input! { n: usize, m: usize, a: [i64; n], b: [i64; m] }
    let mut s : BTreeSet<_> = a.into_iter().enumerate().map(|(i, x)| (x, i)).collect();
    let mut r = 0;
    for &b in &b {
        if let Some(&(x, i)) = s.range((b, 0)..).next() {
            r += x;
            s.remove(&(x, i));
        } else {
            r = -1;
            break;
        }
    }
    println!("{r}");
}
