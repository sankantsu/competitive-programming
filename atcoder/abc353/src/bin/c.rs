use proconio::input;

fn main() {
    input! {
        n: usize,
        mut a: [i64; n],
    }
    a.sort();
    let s: i64 = a.iter().sum();

    let m = 100000000;
    let mut l = 0;
    let mut r = n;
    let mut cnt = 0;
    while l < n {
        r = r.max(l + 1);
        while r - l > 1 && a[l] + a[r - 1] >= m {
            r -= 1;
        }
        cnt += n - r;
        l += 1;
    }
    
    let ans = (n - 1) as i64 * s - cnt as i64 *m;
    println!("{ans}");
}
