use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [[[i64; n]; n]; n],
        q: usize,
        lr: [(usize, usize, usize, usize, usize, usize); q],
    }
    let mut s = vec![vec![vec![0; n + 1]; n + 1]; n + 1];
    for i in 0..n {
        for j in 0..n {
            for k in 0..n {
                s[i + 1][j + 1][k + 1] =
                    s[i + 1][j + 1][k] + s[i + 1][j][k + 1] + s[i][j + 1][k + 1]
                        - s[i + 1][j][k]
                        - s[i][j + 1][k]
                        - s[i][j][k + 1]
                        + s[i][j][k]
                        + a[i][j][k];
            }
        }
    }
    for (mut lx, rx, mut ly, ry, mut lz, rz) in lr {
        lx -= 1;
        ly -= 1;
        lz -= 1;
        let ans = s[rx][ry][rz] - s[lx][ry][rz] - s[rx][ly][rz] - s[rx][ry][lz]
            + s[lx][ly][rz]
            + s[lx][ry][lz]
            + s[rx][ly][lz]
            - s[lx][ly][lz];
        println!("{}", ans);
    }
}
