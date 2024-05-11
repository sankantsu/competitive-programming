use proconio::input;
use proconio::marker::Bytes;

fn main() {
    input! {
        n: usize,
        s: Bytes,
    }
    let mut v = [0; 26];
    let mut l = 0;
    while l < n {
        let mut r = l + 1;
        while r < n && s[r] == s[l] {
            r += 1;
        }
        let idx = (s[l] - b'a') as usize;
        v[idx] = v[idx].max(r - l);
        l = r;
    }
    let ans: usize = v.iter().sum();
    println!("{ans}");
}
