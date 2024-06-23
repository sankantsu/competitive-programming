use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        a: [usize; n],
        mut b: [usize; m],
    }
    b.extend(&a);
    b.sort();
    let ans = b.windows(2).any(|v| v.iter().all(|x| a.contains(x)));
    println!("{}", if ans { "Yes" } else { "No" });
}
