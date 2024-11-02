use proconio::input;

fn square_dist(x1: i64, y1: i64, x2: i64, y2: i64) -> i64 {
    (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
}

fn main() {
    input! {
        x1: i64,
        y1: i64,
        x2: i64,
        y2: i64,
        x3: i64,
        y3: i64,
    }
    let d1 = square_dist(x1, y1, x2, y2);
    let d2 = square_dist(x2, y2, x3, y3);
    let d3 = square_dist(x3, y3, x1, y1);
    let ans = if d1 + d2 == d3 || d2 + d3 == d1 || d3 + d1 == d2 {
        "Yes"
    } else {
        "No"
    };
    println!("{}", ans);
}
