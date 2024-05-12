use proconio::input;

fn main() {
    input! {
        n: i64,
        h: [i64; n],
    }
    let mut ans = -1;
    for i in 1..n {
        if h[i as usize] > h[0] {
            ans = i + 1;
            break;
        }
    }
    println!("{ans}");
}
