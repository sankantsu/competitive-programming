use proconio::input;

fn main() {
    input! {
        n: usize,
        h: [usize; n],
    }
    let mut v = vec![0; n];
    let mut st = vec![];
    for i in (0..n).rev() {
        loop {
            if st.is_empty() {
                st.push(i);
                break;
            }
            let j = *st.last().unwrap();
            if h[j] >= h[i] {
                st.push(i);
                break;
            }
            v[j] = i;
            st.pop();
        }
    }
    let mut cnt = vec![0; n];
    for i in 0..n {
        cnt[v[i]] += 1;
    }
    let mut ans = 0;
    for i in 0..n {
        ans += cnt[i] - 1;
        print!("{} ", ans);
    }
    println!();
}
