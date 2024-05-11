use std::collections::BTreeSet;
use std::collections::HashMap;
use proconio::input;

fn main() {
    input! {
        n: usize,
        q: usize,
        mut a: [i64; n],
        mut ix: [(usize, i64); q],
    }
    for (i, _) in &mut ix {
        *i -= 1;
    }

    let mx = 200000;
    let mut mex = BTreeSet::new();
    for x in 0..=mx {
        mex.insert(x);
    }
    
    let mut cnt = HashMap::new();
    for &x in &a {
        match cnt.get(&x) {
            Some(v) => {
                cnt.insert(x, v+1);
            },
            None => {
                cnt.insert(x, 1);
                mex.remove(&x);
            }
        }
    }

    for &(i, x) in &ix {
        let old = a[i];
        let v = cnt.get(&old).unwrap();
        // remove old value
        if *v == 1 {
            cnt.remove(&old);
            mex.insert(old);
        } else {
            cnt.insert(old, *v - 1);
        }
        // insert new value
        a[i] = x;
        if mex.contains(&x) {
            mex.remove(&x);
            cnt.insert(x, 1);
        } else {
            match cnt.get(&x) {
                Some(v) => cnt.insert(x, v + 1),
                None => cnt.insert(x, 1),
            };
        }
        let ans = mex.first().unwrap();
        println!("{}", ans);
    }
}
