use proconio::input;

fn main() {
    input! {
        n: usize,
        c: i64,
        t: [i64; n],
    }
    let mut l = 0 - c;
    let mut ans = 0;
    for t in t {
        if t - l >= c {
            ans += 1;
            l = t;
        }
    }
    println!("{}", ans);
}
