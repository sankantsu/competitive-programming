use proconio::input;

fn main() {
    input! {
        n: usize,
        k: usize,
        x: i64,
        mut a: [i64; n],
    }
    a.insert(k, x);
    for v in &a {
        print!("{} ", v);
    }
    println!();
}
