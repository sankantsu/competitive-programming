use proconio::input;
use proconio::marker::Chars;

// Q.
// ABA -> A, BAB -> B の置き変えが可能
// つくれる文字列の種類数は?
//
// 考察
//
// ABABABAB... のように A, B が交互に続いている文字列は、どこを選んで置きかえても
// 連続する AB がひとつ消えて同じペアになるだけ
// 交互に並んでいる部分が n 文字あるとすると置きかえは最大で (n - 1)/2 回まで置きかえ可能
// -> (0 回の置きかえも含めて) p(n) = (n - 1)/2 + 1 パターンできる
//
// AA, BB のように同じ文字が連続する箇所は置きかえできないので、必ず残る
//
// 交互に並んでいる箇所ごとに分割して、各部分で何パターンできるか数えて掛け算すれば良さそう?
//
// e.g. BBABABAABABAAAABA
//    B|BABABA|ABABA|A|A|ABA
// n: 1|  6   |  5  |1|1| 3
// p: 1 * 3 * 3 * 1 * 1 * 2 = 18

fn main() {
    input! {
        n: usize,
        s: Chars,
    }
    let mut v = vec![];
    let mut l = 0;
    let mut r = l + 1;
    while r < n {
        while r < n && s[r - 1] != s[r] {
            r += 1;
        }
        v.push(r - l);
        l = r;
        r = r + 1;
    }
    let mut ans: usize = 1;
    for x in &v {
        ans *= (x - 1) / 2 + 1;
        ans = ans % 1000000007;
    }
    println!("{ans}");
}
