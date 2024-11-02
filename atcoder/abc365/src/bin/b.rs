use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [i64; n],
    }
    let mut v = a
        .iter()
        .enumerate()
        .map(|(i, x)| (x, i + 1))
        .collect::<Vec<_>>();
    v.sort();
    let ans = v[v.len() - 2].1;
    println!("{ans}");
}
