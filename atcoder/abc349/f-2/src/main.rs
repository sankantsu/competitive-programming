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
    let mut cnt = vec![mint::new(0); 1<<k];
    for &x in &a {
        if m%x != 0 {
            continue;
        }
        let r = bit_repr(x, &fs);
        cnt[r as usize] += 1;
    }

    // zeta transform
    let mut sum = cnt;
    for i in 0..k {
        for j in 0..1<<k {
            if j & (1<<i) == 0 {
                sum[j|(1<<i)] = sum[j|(1<<i)] + sum[j];
            }
        }
    }

    let h = sum.iter().map(|x| mint::pow(mint::new(2), x.val() as u64)).collect::<Vec<_>>();
    // mobius transform
    let mut g = h;
    for i in 0..k {
        for j in 0..1<<k {
            if j & (1<<i) == 0 {
                g[j|(1<<i)] = g[j|(1<<i)] - g[j];
            }
        }
    }

    let mut ans = g[(1<<k) - 1];
    if m == 1 {
        ans -= 1;
    }
    println!("{}", ans);
}
