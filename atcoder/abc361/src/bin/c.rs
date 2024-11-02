use proconio::input;

fn main() {
    input! {
        n: usize,
        k: usize,
        mut a: [i64; n],
    }
    a.sort();
    let m = n - k;
    let mut ans = a[n - 1] - a[0];
    for i in 0..=k {
        let v = a[i + m - 1] - a[i];
        if v < ans {
            ans = v;
        }
    }
    println!("{}", ans);
}
