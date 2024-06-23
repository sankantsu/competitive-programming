use proconio::input;

// はじめて A_i > 0 となる時刻
// H_p1 > H_p2 > ... > H_pk = H_i
// としたとき、求める数は、
// H_p1 * (p1 + 1) + H_p2 * (p2 - p1) + ... + H_pk * (p_k - p_k-1)
// となるはず
// 各 i を始点として逆向きに狭義単調増加列を見つければ良い
// 各 i について、j < i かつ H_j > H_i なる最も大きい j がわかれば良い

fn main() {
    input! {
        n: usize,
        mut h: [usize; n],
    }

    let mut exceeder = vec![!0; n];
    let mut st = vec![];
    for i in (0..n).rev() {
        while !st.is_empty() {
            let t = *st.last().unwrap();
            if h[t] < h[i] {
                exceeder[t] = i;
                st.pop();
            } else {
                break;
            }
        }
        st.push(i);
    }

    let mut ans = vec![];
    for i in 0..n {
        let x;
        if exceeder[i] == !0 {
            x = h[i] * (i + 1);
        } else {
            let u = h[i] * (i - exceeder[i]);
            let v = ans[exceeder[i]];
            x = u + v;
        }
        print!("{} ", x + 1);
        ans.push(x);
    }
    println!();
}
