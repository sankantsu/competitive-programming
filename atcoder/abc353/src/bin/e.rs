use std::collections::HashMap;
use proconio::input;
use proconio::marker::Chars;

fn main() {
    input! {
        n: usize,
        s: [Chars; n],
    }
    let mut mp = HashMap::new();
    let ms = [ 998244353, 1000000007, 1000000009, 1000000021, 1000000033 ];
    let b = 200;
    let mut ans = 0;
    for s in &s {
        let mut hash = vec![0; ms.len()];
        for c in s {
            hash = hash.iter().enumerate().map(|(i, h)| (b * h + *c as usize) % ms[i]).collect();
            let v = *mp.get(&hash).unwrap_or(&0usize);
            ans += v;
            mp.insert(hash.clone(), v+1);
        }
    }
    println!("{ans}");
}
