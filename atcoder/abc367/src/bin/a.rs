use proconio::input;

fn main() {
    input! {
        a: usize,
        b: usize,
        c: usize,
    }
    let ans = if a <= b && b <= c || b <= c && c <= a || c <= a && a <= b {
        "Yes"
    } else {
        "No"
    };
    println!("{}", ans);
}
