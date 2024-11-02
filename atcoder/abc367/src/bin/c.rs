use itertools::Itertools;
use proconio::input;

fn main() {
    input! { n: usize, k: usize, r: [usize; n] }
    for l in (0..n).map(|i| 1..=r[i]).multi_cartesian_product() {
        if l.iter().sum::<usize>() % k == 0 {
            println!("{}", l.iter().join(" "));
        }
    }
}
