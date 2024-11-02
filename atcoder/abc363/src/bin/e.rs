use proconio::input;
use std::collections::VecDeque;

fn main() {
    input! {
        h: usize,
        w: usize,
        y: usize,
        a: [[usize; w]; h],
    }
    let di = [!0, 0, 1, 0];
    let dj = [0, !0, 0, 1];
    let mut sink = vec![vec![false; w]; h];
    let mut h_to_pos = vec![vec![]; y + 1];
    for i in 0..h {
        for j in 0..w {
            if a[i][j] > y {
                continue;
            }
            h_to_pos[a[i][j]].push((i, j));
        }
    }
    let mut remain = h * w;
    for t in 1..=y {
        for &(i, j) in &h_to_pos[t] {
            if sink[i][j] {
                continue;
            }
            let mut can_sink = false;
            if i == 0 || i == h - 1 || j == 0 || j == w - 1 {
                can_sink = true;
            } else {
                for dir in 0..4 {
                    let ni = usize::wrapping_add(i, di[dir]);
                    let nj = usize::wrapping_add(j, dj[dir]);
                    if sink[ni][nj] {
                        can_sink = true;
                    }
                }
            }
            if !can_sink {
                continue;
            }
            let mut queue = VecDeque::new();
            queue.push_back((i, j));
            sink[i][j] = true;
            while !queue.is_empty() {
                let (ci, cj) = queue.pop_front().unwrap();
                remain -= 1;
                for dir in 0..4 {
                    let ni = usize::wrapping_add(ci, di[dir]);
                    let nj = usize::wrapping_add(cj, dj[dir]);
                    if ni == !0 || ni == h || nj == !0 || nj == w {
                        continue;
                    }
                    if sink[ni][nj] {
                        continue;
                    }
                    if a[ni][nj] > t {
                        continue;
                    }
                    queue.push_back((ni, nj));
                    sink[ni][nj] = true;
                }
            }
        }
        println!("{}", remain);
    }
}
