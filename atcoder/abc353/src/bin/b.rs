use proconio::input;

fn main() {
    input! {
        n: usize,
        k: i64,
        a: [i64; n],
    }
    let mut i = 0;
    let mut cnt = 0;
    while i < n {
        let mut cap = k;
        while i < n && a[i] <= cap {
            cap -= a[i];
            i += 1;
        }
        cnt += 1;
    }
    println!("{cnt}");
}
