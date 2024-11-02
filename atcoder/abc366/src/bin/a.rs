use proconio::input;

fn main() {
    input! {
        n: usize,
        t: usize,
        a: usize,
    }
    println!("{}", if t > n / 2 || a > n / 2 { "Yes" } else { "No" });
}
