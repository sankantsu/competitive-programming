fn main() {
    proconio::input! {
        s: String,
        t: String,
    }

    let mut i = 0;
    let mut v : Vec<usize> = vec![];
    for c in s.as_bytes() {
        while &t.as_bytes()[i] != c {
            i += 1;
        }
        v.push(i+1);
        i += 1;
    }

    for p in v {
        print!("{} ", p);
    }
    println!();
}
