use proconio::input;
use proconio::marker::Chars;
use ac_library::modint::ModInt998244353 as mint;

// K 文字の部分文字列すべてが回文にならないようにしたい
// i 文字目を加えて回文になるかどうかは、[i-K, i) 文字ぶん覚えておけば判定できる。
// 最後 K - 1 文字の組み合わせ (最大 2**9 = 512 通り) が同じものを同一視して dp すれば良さそう?
//
// dp[i][s] = (先頭 i 文字としてありうる文字列のうち、末尾 K - 1 文字が s に一致していて、長さ K の回文をひとつも含まない文字列の数)
// とすると、
// (i) c が i + 1 番目の文字として不適なとき (固定されている文字に一致しないとき) 遷移なし
// (ii) s の末尾に c を足した文字列が回文のとき遷移なし
// (iii) s の末尾に c を足した文字列が回文ではないとき、dp[i+1][s_2..s_k-1 c] += dp[i][s]
//
// 遷移をナイーブに計算したとき、回文判定および文字列のコピーに O(K) ~ 10
// 遷移の回数は N * 2^(K - 1) * 2 ~  10^6

fn dbg_str(x: usize, k: usize) -> String {
    let mut s = String::new();
    for i in 0..k {
        if ((x >> i) & 1) == 0 {
            s.push('A');
        } else {
            s.push('B');
        }
    }
    s
}

fn is_palindrome(x: usize, k: usize) -> bool {
    (0..(k / 2)).all(|i| ((x>>i) & 1) == ((x >> (k - 1 - i)) & 1))
}

fn main() {
    input! {
        n: usize,
        k: usize,
        s: Chars,
    }
    let mut dp = vec![vec![mint::new(0); usize::pow(2, (k - 1) as u32)]; n + 1];
    // first k - 1 characters
    for t in 0..(1 << (k - 1)) {
        let mut ok = true;
        for i in 0..(k - 1) {
            let c = (t>>i) & 1;
            if (c == 0 && s[i] =='B') || (c == 1 && s[i] == 'A') {
                ok = false;
                break;
            }
        }
        if ok {
            //dbg!(dbg_str(t, k - 1));
            dp[k - 1][t] = mint::new(1);
        }
    }
    for i in (k - 1)..n {
        for t in 0..(1 << (k - 1)) {
            for c in [0, 1] {
                if (c == 0 && s[i] == 'B') || (c == 1 && s[i] == 'A') { continue; }
                let u = t | (c << (k - 1));
                if is_palindrome(u, k) { continue; }
                let tmp = dp[i][t];
                //dbg!(i, s[i], dbg_str(t, k - 1), c, dbg_str(u>>1, k - 1), tmp);
                dp[i + 1][u >> 1] += tmp;
            }
        }
    }
    let ans = dp[n].iter().sum::<mint>().val();
    println!("{ans}");
}
