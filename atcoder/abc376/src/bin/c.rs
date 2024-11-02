use proconio::input;

fn main() {
    input! {
        n: usize,
        mut a: [usize; n],
        mut b: [usize; n - 1],
    }
    let mut ans = -2;
    a.sort();
    b.sort();
    for w in a.iter().rev() {
        if !b.is_empty() && w <= b.last().unwrap() {
            b.pop().unwrap();
            continue;
        }
        if ans == -2 {
            ans = *w as i64;
        } else {
            ans = -1;
            break;
        }
    }
    println!("{}", ans);
}
