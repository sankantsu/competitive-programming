use proconio::input;

fn main() {
    input! {
        n: usize,
        k: i64,
        xy: [(i64, i64); n],
    }
    let mut x = vec![];
    let mut y = vec![];
    for &(a, b) in &xy {
        x.push(a);
        y.push(b);
    }
    x.sort();
    y.sort();

    fn merge<T: PartialOrd + Copy>(x: &Vec<T>, y: &Vec<T>) -> Vec<T> {
        let mut res = vec![];
        let mut i = 0;
        let mut j = 0;
        while i < x.len() {
            if j == y.len() {
                while i < x.len() {
                    res.push(x[i]);
                    i += 1;
                }
            } else if x[i] < y[j] {
                res.push(x[i]);
                i += 1;
            } else {
                res.push(y[j]);
                j += 1;
            }
        }
        res
    }

    fn cost(r: i64, x: &Vec<i64>) -> i64 {
        let n = x.len();
        let mut xs = vec![];
        for &x in x {
            xs.push(x - r);
        }
        let y = merge(x, &xs);
        let v = y[n - 1];
        let mut ans = 0;
        for &x in x {
            if x < v {
                ans += v - x;
            } else if v + r < x {
                ans += x - (v + r);
            }
        }
        ans
    }

    let check = |r: i64| {
        let d = cost(r, &x) + cost(r, &y);
        d <= k
    };

    let mut ok: i64 = 1<<32;
    let mut ng: i64 = -1;
    while ok - ng > 1 {
        let mid = (ok + ng) / 2;
        if check(mid) {
            ok = mid;
        } else {
            ng = mid;
        }
    }
    println!("{}", ok);
}
