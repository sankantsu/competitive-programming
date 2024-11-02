use proconio::input;
use std::cmp::Ordering;

const M: usize = 1000000;

fn calc_dist_for_all_coordinate(n: usize, x: &[i64]) -> Vec<i64> {
    let mut v = vec![0; 4 * M + 1];
    v[0] = x.iter().sum::<i64>() + (2 * M * n) as i64;

    let mut cnt = vec![0; 4 * M + 1];
    for x in x {
        cnt[(*x + 2 * M as i64) as usize] += 1;
    }

    let mut k = -1 * n as i64;
    for i in 0..4 * M {
        k += 2 * cnt[i];
        v[i + 1] = v[i] + k;
    }
    v
}

fn main() {
    input! {
        n: usize,
        d: i64,
        xy: [(i64, i64); n],
    }
    let (x, y): (Vec<_>, Vec<_>) = xy.iter().cloned().unzip();

    let vx = calc_dist_for_all_coordinate(n, &x);
    let mut vy = calc_dist_for_all_coordinate(n, &y);
    vy.sort();

    let mut ans = 0;
    for dx in vx {
        let k = vy
            .binary_search_by(|dy| {
                if *dy > d - dx {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            })
            .unwrap_err();
        ans += k;
    }
    println!("{}", ans);
}

