use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        a: [usize; n],
    }
    let s: usize = a.iter().sum();
    if s <= m {
        println!("infinite");
        return;
    }
    let check = |x| {
        let s: usize = a.iter().map(|y| std::cmp::min(*y, x)).sum();
        s <= m
    };
    let mut ok = 0;
    let mut ng = m + 1;
    while ng - ok > 1 {
        let mid = (ok + ng) / 2;
        if check(mid) {
            ok = mid;
        } else {
            ng = mid;
        }
    }
    println!("{ok}");
}
