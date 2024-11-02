use proconio::input;

fn main() {
    input! {
        r: usize,
        g: usize,
        b: usize,
        c: String,
    }
    let ans = if c == "Red" {
        usize::min(g, b)
    } else if c == "Green" {
        usize::min(r, b)
    } else {
        usize::min(r, g)
    };
    println!("{}", ans);
}
