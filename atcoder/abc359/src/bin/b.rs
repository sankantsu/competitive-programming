use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; 2*n],
    }
    let mut ans = 0;
    for c in 1..=n {
        let v: Vec<_> = a.iter().enumerate().filter(|(_, x)| **x == c).map(|(i, _)| i).collect();
        if v[1] - v[0] == 2 {
            ans += 1;
        }
    }
    println!("{ans}");
}
