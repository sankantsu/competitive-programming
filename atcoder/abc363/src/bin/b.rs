use proconio::input;

fn main() {
    input! {
        n: usize,
        t: i64,
        p: usize,
        mut l: [i64; n],
    }
    l.sort();
    let ans = i64::max(0, t - l[n - p]);
    println!("{}", ans);
}
