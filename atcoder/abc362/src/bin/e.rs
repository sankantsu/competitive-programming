use ac_library::modint::ModInt998244353 as mint;
use proconio::input;

// 前から順番に範囲を伸ばして dp すると良さそう
// 計算量は N^4 まで OK
//
// dp[i][j][k] = (最後の項が A_j, 長さ j, 公差 (a_i - a_k) の等差数列の数) (k < i)
//
// j = 1, 2 は常に等差数列なので、特別扱い
// dp[i][j][k] = \sum_l dp[k][j - 1][l] for l < k < i, a_k - a_l = a_i - a_k
//
// テーブルサイズ N^3 で、1 エントリあたり O(N)?

fn main() {
    input! {
        n: usize,
        a: [i64; n],
    }
    let mut dp = vec![vec![vec![mint::new(0); n]; n + 1]; n];
    for i in 0..n {
        for j in 1..=n {
            for k in 0..i {
                if j <= 2 {
                    dp[i][j][k] = mint::new(1);
                } else {
                    for l in 0..k {
                        if a[i] - a[k] == a[k] - a[l] {
                            let tmp = dp[k][j - 1][l];
                            dp[i][j][k] += tmp;
                        }
                    }
                }
            }
        }
    }
    let mut ans = vec![mint::new(0); n + 1];
    for i in 0..n {
        for j in 1..=n {
            for k in 0..n {
                ans[j] += dp[i][j][k];
            }
        }
    }
    ans[1] = mint::new(n);

    for i in 1..=n {
        print!("{} ", ans[i].val());
    }
    println!();
}
