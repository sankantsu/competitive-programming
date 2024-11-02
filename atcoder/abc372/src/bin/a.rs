use proconio::input;

fn main() {
    input! {
        s: String,
    }
    let mut t = String::new();
    for c in s.chars() {
        if c == '.' {
            continue;
        }
        t.push(c);
    }
    println!("{}", t);
}
