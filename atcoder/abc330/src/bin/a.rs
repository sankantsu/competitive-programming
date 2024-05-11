use proconio::input;

fn main() {
    input! {
        n: usize,
        l: usize,
        a: [usize; n],
    }

    let mut ans = 0;
    for &x in &a {
        if x >= l {
            ans += 1;
        }
    }
    println!("{}", ans);
}
