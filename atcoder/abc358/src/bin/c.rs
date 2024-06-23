use proconio::{*, marker::*};

fn main() {
    input! { n: usize, m: usize, s: [Chars; n] }
    let mut ans = n;
    for x in 0usize..(1<<n) {
        let mut t = 0;
        for i in 0..n {
            if (x&(1<<i)) == 0 {
                continue;
            }
            for j in 0..m {
                if s[i][j] == 'o' {
                    t |= 1<<j;
                }
            }
        }
        if t == (1<<m) - 1 {
            ans = usize::min(ans, x.count_ones() as usize);
        }
    }
    println!("{ans}");
}
