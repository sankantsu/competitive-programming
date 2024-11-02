use proconio::input;

fn main() {
    input! {
        n: usize,
        k: i64,
        mut p: [usize; n],
    }
    for i in 0..n {
        p[i] -= 1;
    }

    let mut q = vec![0; n];
    for i in 0..n {
        q[p[i]] = i as i64;
    }

    let mut ans = vec![];
    for x in 0..n {
        let mut v = vec![];
        for i in 0..x {
            if q[i] - q[x] >= k {
                v.push(i);
            }
        }
        v.push(x);
        for i in (0..(v.len() - 1)).rev() {
            ans.push((q[v[i + 1]], q[v[i]]));
            q.swap(v[i + 1], v[i]);
        }
    }

    println!("{}", ans.len());
    for (i, j) in ans {
        println!("{} {}", i + 1, j + 1);
    }
}
