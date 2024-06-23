use proconio::input;

fn main() {
    input! {
        n: usize,
        mut m: i64,
        h: [i64; n],
    }
    let mut cnt = 0;
    for h in &h {
        if *h <= m {
            cnt += 1;
        }
        m -= *h;
    }
    println!("{cnt}");
}
