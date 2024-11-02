use proconio::input;

#[allow(unused)]
fn solve1(n: usize, a: &[usize]) -> usize {
    let max_bit = 32;
    let mut ans = 0;
    for k in 0..max_bit {
        let v: Vec<_> = a.iter().map(|x| (x >> k) & 0x1).collect();
        let mut s = vec![0; n + 1];
        for i in 0..n {
            s[i + 1] += s[i] ^ v[i];
        }
        let cnt = s.iter().filter(|s| **s == 1).count();
        let t: usize = v.iter().sum();
        ans += (cnt * (n + 1 - cnt) - t) * (1 << k);
    }
    ans
}

fn solve2(n: usize, a: &[usize]) -> usize {
    let max_bit = 32;
    let mut ans = 0;
    for k in 0..max_bit {
        let v: Vec<_> = a.iter().map(|x| (x >> k) & 0x1).collect();
        let mut dp = vec![vec![0; 2]; 3];
        dp[0][0] = 1;

        for i in 0..n {
            let mut ndp = vec![vec![0; 2]; 3];
            for j in 0..3 {
                for k in 0..2 {
                    for nj in 0..3 {
                        if nj < j {
                            continue;
                        }
                        let nk = k ^ (if nj == 1 && v[i] == 1 { 1 } else { 0 });
                        ndp[nj][nk] += dp[j][k];
                    }
                }
            }
            dp = ndp;
        }

        let t: usize = v.iter().sum();
        ans += (dp[1][1] + dp[2][1] - t) * (1 << k);
    }
    ans
}

fn main() {
    input! {
        n: usize,
        a: [usize; n],
    }

    // let ans = solve1(n, &a);
    let ans = solve2(n, &a);

    println!("{}", ans);
}
