use proconio::input;

fn main() {
    input! {
        sx: i64,
        sy: i64,
        tx: i64,
        ty: i64,
    }
    let dy = (ty - sy).abs();
    let xmin;
    let xmax;
    if (sx + sy) % 2 == 0 {
        xmin = sx - dy;
        xmax = sx + dy + 1;
    } else {
        xmin = sx - dy - 1;
        xmax = sx + dy;
    }
    let ans;
    if xmin <= tx && tx <= xmax {
        ans = dy;
    } else if tx < xmin {
        ans = dy + (xmin - tx + 1)/2;
    } else {
        ans = dy + (tx - xmax + 1)/2;
    }
    println!("{ans}");
}
