use proconio::input;
use ac_library::ModInt998244353 as mint;

fn main() {
    input! {
        n: usize,
    }
    let m = n.to_string().len();
    let r = mint::new(10).pow(m as u64);
    let ans = mint::new(n) * (r.pow(n as u64) - 1) / (r - 1);
    println!("{}", ans.val());
}
