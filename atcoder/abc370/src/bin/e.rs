use ac_library::modint::ModInt998244353 as mint;
use proconio::input;
use std::collections::HashMap;

fn main() {
    input! { n: usize, k: i64, a: [i64; n] }
    let mut s = 0;
    let mut dp = vec![mint::new(1)];
    let mut sum = mint::new(1);
    let mut m = HashMap::new();
    m.insert(0, mint::new(1));
    for i in 0..n {
        s += a[i];
        let mut x = sum;
        x -= *m.get(&(s - k)).unwrap_or(&mint::new(0));
        *m.entry(s).or_insert(mint::new(0)) += x;
        dp.push(x);
        sum += x;
    }
    let ans = dp.pop().unwrap();
    println!("{}", ans.val());
}
