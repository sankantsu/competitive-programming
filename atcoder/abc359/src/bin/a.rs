use proconio::input;

fn main() {
    input! {
        n: usize,
        s: [String; n],
    }
    let cnt = s.iter().filter(|s| *s == "Takahashi").count();
    println!("{cnt}");
}
