fn main() {
    proconio::input! {
        n: usize,
        ab: [(usize, usize); n],
    }

    let mut s = 0;
    for (a, _) in ab.iter() {
        s += a;
    }

    let mut mx = 0;
    for (a, b) in ab.iter() {
        if b - a > mx {
            mx = b - a;
        }
    }
    s += mx;

    println!("{}", s)
}
