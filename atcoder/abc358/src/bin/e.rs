use proconio::input;
use ac_library::ModInt998244353 as mint;

const N: usize = 1001;
const MOD: usize = 998244353;

fn init(fact: &mut [mint], inv: &mut [mint], finv: &mut [mint]) {
    fact[0] = mint::new(1);
    fact[1] = mint::new(1);
    inv[1] = mint::new(1);
    finv[0] = mint::new(1);
    finv[1] = mint::new(1);
    for i in 2..N {
        fact[i] = mint::new(i) * fact[i - 1];
        inv[i] = -inv[MOD%i] * mint::new(MOD/i);
        finv[i] = inv[i] * finv[i - 1];
    }
}

fn main() {
    let mut fact = [mint::new(0); N];
    let mut inv = [mint::new(0); N];
    let mut finv = [mint::new(0); N];
    init(&mut fact, &mut inv, &mut finv);

    let comb = |n, k| {
        fact[n] * finv[k] * finv[n - k]
    };

    input! {
        k: usize,
        c: [usize; 26],
    }
    let mut dp = [[mint::new(0); N]; 26 + 1];
    dp[0][0] = mint::new(1);
    for i in 0..26 {
        for j in 0..N {
            for l in 0..=usize::min(j, c[i]) {
                dp[i+1][j] += dp[i][j - l] * comb(j, l);
            }
        }
    }
    let ans: mint = dp[26][1..=k].iter().sum();
    println!("{}", ans.val());
}
