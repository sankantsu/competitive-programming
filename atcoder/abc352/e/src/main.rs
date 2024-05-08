use std::collections::BinaryHeap;
use ac_library::dsu::Dsu;

fn main() {
    proconio::input! {
        n: usize,
        m: usize,
    }

    let mut a = vec![];
    let mut bh = BinaryHeap::new();
    for i in 0..m {
        proconio::input! {
            k: usize,
            c: i64,
        }
        proconio::input! {
            mut b: [usize; k],
        }
        for x in &mut b {
            *x -= 1
        }
        a.push(b);
        bh.push((-c, i));
    }

    let mut ans = 0;
    let mut uf = Dsu::new(n);
    while !bh.is_empty() {
        if let Some((nc, i)) = bh.pop() {
            let c = -nc;
            let b = &a[i];
            for j in 1..b.len() {
                if !uf.same(b[0], b[j]) {
                    ans += c;
                    uf.merge(b[0], b[j]);
                }
            }
        }
    }

    let mut con = true;
    for i in 1..n {
        if !uf.same(0, i) {
            con = false;
        }
    }

    if con {
        println!("{}", ans);
    } else {
        println!("{}", -1);
    }
}
