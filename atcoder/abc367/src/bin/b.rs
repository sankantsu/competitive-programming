use proconio::input;

fn main() {
    input! {
        mut x: String,
    }
    while x.ends_with('0') {
        x.pop();
    }
    println!("{}", x.trim_end_matches('.'));
}
