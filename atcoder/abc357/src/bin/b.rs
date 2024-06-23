use proconio::input;

fn main() {
    input! {
        s: String,
    }
    let small = s.chars().filter(|&c| c.is_lowercase()).count();
    let cap = s.chars().count() - small;
    println!("{}", if cap > small { s.to_uppercase() } else { s.to_lowercase() });
}
