use proconio::input;

fn main() {
    input! {
        n: usize,
        q: usize,
    }
    let mut ans = 0;
    let mut l = 1;
    let mut r = 2;
    for _ in 0..q {
        input! {
            h: char,
            t: usize,
        }
        if h == 'L' {
            if l <= r && r <= t || t <= r && r <= l {
                ans += n - (usize::max(l, t) - usize::min(l, t));
            } else {
                ans += usize::max(l, t) - usize::min(l, t);
            }
            l = t;
        }
        if h == 'R' {
            if r <= l && l <= t || t <= l && l <= r {
                ans += n - (usize::max(r, t) - usize::min(r, t));
            } else {
                ans += usize::max(r, t) - usize::min(r, t);
            }
            r = t;
        }
    }
    println!("{}", ans);
}
