use proconio::input;

fn main() {
    input! {
        l: i32,
        r: i32,
    }
    let ans = if l == 1 && r == 0 {
        "Yes"
    } else if l == 0 && r == 1 {
        "No"
    } else {
        "Invalid"
    };
    println!("{}", ans);
}
