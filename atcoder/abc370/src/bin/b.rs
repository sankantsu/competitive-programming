use proconio::input;

fn main() {
    input! {
        n: usize,
    }
    let mut a = vec![];
    for i in 1..=n {
        let mut v = vec![];
        for _ in 0..i {
            input! { mut x: usize }
            x -= 1;
            v.push(x);
        }
        a.push(v);
    }
    let mut c = 0;
    for i in 0..n {
        let next = if c >= i { a[c][i] } else { a[i][c] };
        c = next;
    }
    let ans = c + 1;
    println!("{}", ans);
}
