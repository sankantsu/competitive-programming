use ac_library::modint::ModInt998244353 as mint;
use proconio::input;

// まず、1 番左以外は同じ確率になるはずだから
// 1 番左とそれ以外で分けて考えれば良い。
//
// 黒玉が絡まない交換は無視して良い
//
// 黒玉が絡む交換が発生する確率は 2(N - 1)/N^2
// 黒が 1 番左にあるとき、交換が発生したら必ず 1 番左以外に移動する
// 黒が 1 番左以外にあるとき、1 番左に戻るような交換は 2 パターンだけ
//
// 黒が 1 番左にある状態を S_1, それ以外にある状態を S_2 とすると、
// S_1 -> S_1: 1 - p
// S_1 -> S_2: p = 2(N - 1)/N^2
// S_2 -> S_1: q = 2/N^2
// S_2 -> S_2: 1 - q
//
// P(S_2) = 1 - P(S_1)
// P(S_1)(i + 1) = P(S_1)(i) * (1 - p) + P(S_2) * q
//
// ans = 1 * P(S_1) + (2 + 3 + .. + n) * P(S_2)/(n - 1)
// 2 + 3 + .. + n = n*(n + 1)/2 - 1
//
// 漸化式に従ってナイーブに S_1, S_2 をシミュレーションしていっても間にあいそう。

fn main() {
    input! {
        n: usize,
        k: usize,
    }
    let mut s = mint::new(1); // S_1 にいる確率
    let p = mint::new(2 * (n - 1)) / mint::new(n * n); // S_1 -> S_2
    let q = mint::new(2) / mint::new(n * n); // S_2 -> S_1
    for _ in 0..k {
        s = s * (mint::new(1) - p) + (mint::new(1) - s) * q;
    }

    let ans = if n == 1 {
        s
    } else {
        mint::new(1) * s + mint::new(n * (n + 1) / 2 - 1) * (mint::new(1) - s) / (mint::new(n) - 1)
    };
    println!("{ans}");
}
