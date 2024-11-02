use proconio::input;

fn main() {
    input! {
        r: i64,
    }
    let ans = 100 - r % 100;
    println!("{}", ans);
}
