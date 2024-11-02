use proconio::input;
use std::collections::HashMap;

fn main() {
    input! {
        q: usize,
    }
    let mut cnt = HashMap::new();
    for _ in 0..q {
        input! { t: usize }
        if t == 1 {
            input! { x: usize }
            *cnt.entry(x).or_insert(0) += 1;
        } else if t == 2 {
            input! { x: usize }
            *cnt.get_mut(&x).unwrap() -= 1;
            if cnt[&x] == 0 {
                cnt.remove(&x);
            }
        } else {
            println!("{}", cnt.len());
        }
    }
}
