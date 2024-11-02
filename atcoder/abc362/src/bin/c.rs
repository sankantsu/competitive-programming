use proconio::input;

fn main() {
    input! {
        n: usize,
        lr: [(i64, i64); n],
    }
    let mn: i64 = lr.iter().map(|(l, _)| l).sum();
    let mx: i64 = lr.iter().map(|(_, r)| r).sum();
    if mn <= 0 && 0 <= mx {
        println!("Yes");
        let mut e = 0 - mn;
        let mut v = vec![];
        for i in 0..n {
            let (l, r) = lr[i];
            if r - l < e {
                v.push(r);
                e -= r - l;
            } else {
                v.push(l + e);
                e = 0;
            }
        }
        for x in v {
            print!("{} ", x);
        }
        println!();
    } else {
        println!("No");
    }
}
