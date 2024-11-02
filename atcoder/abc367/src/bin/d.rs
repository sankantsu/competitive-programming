use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        a: [usize; n],
    }
    let mut s = vec![0; n + 1];
    for i in 0..n {
        s[i + 1] = (s[i] + a[i]) % m;
    }
    let t = a.iter().sum::<usize>() % m;
    let mut ans = 0usize;
    let mut cnt = vec![0usize; m];
    for i in 0..n {
        let x = s[i];
        let y = (m + x - t) % m;
        ans += cnt[x] + cnt[y];
        cnt[x] += 1;
    }
    println!("{}", ans);
}
