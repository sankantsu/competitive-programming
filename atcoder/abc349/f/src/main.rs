use ac_library::modint::ModInt998244353 as mint;

fn factorize(mut x: i64) -> Vec<i64> {
    let mut res = vec![];
    let mut p = 2;
    while p*p <= x {
        let mut q = 1;
        while x % p == 0 {
            q *= p;
            x /= p;
        }
        if q != 1 {
            res.push(q);
        }
        p += 1;
    }
    if x != 1 {
        res.push(x);
    }
    res
}

fn bit_repr(x: i64, v: &Vec<i64>) -> i32 {
    let mut k = 0;
    for (i,q) in v.iter().enumerate() {
        if x % q == 0 {
            k |= 1<<i;
        }
    }
    k
}

fn main() {
    proconio::input! {
        n: usize,
        m: i64,
        a: [i64; n],
    }

    let fs = factorize(m);
    let k = fs.len();
    let mx = usize::pow(2, k as u32);
    let mut cnt = vec![0; mx];
    for &x in &a {
        if m%x != 0 {
            continue;
        }
        let r = bit_repr(x, &fs);
        cnt[r as usize] += 1;
    }

    let mut dp = vec![mint::new(0); mx];
    dp[0] = mint::new(1);
    for i in 0..mx {
        let mut ndp = vec![mint::new(0); mx];
        let p = mint::pow(mint::new(2), cnt[i]) - 1;
        for j in 0..mx {
            let x = dp[j] * p;
            ndp[j] += dp[j];
            ndp[i|j] += x;
        }
        dp = ndp;
    }
    let mut ans = dp[mx-1].val();
    if m == 1 {
        ans -= 1;
    }
    println!("{}", ans);
}
