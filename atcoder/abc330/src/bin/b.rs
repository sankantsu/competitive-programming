use proconio::input;

fn main() {
    input! {
        n: usize,
        l: i64,
        r: i64,
        a: [i64; n],
    }
    for &x in &a {
        let ans;
        if l <= x && x <= r {
            ans = x;
        } else if (l - x).abs() < (r - x).abs() {
            ans = l;
        } else {
            ans = r;
        }
        print!("{} ", ans);
    }
    println!();
}
