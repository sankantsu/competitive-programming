use itertools::Itertools;
use proconio::input;

// 回文数
//
// K ケタの回文数はいくつ?
//
// - K が偶数のとき
// K/2 個の任意の整数の並びをつくって、反転させたものをくっつければ回文数がつくれる
// 最初の数字だけは 1-9, それ以外は 0-9
// -> 9 * 10^(K/2 - 1)
//
// - K が奇数のとき
// (K + 1) / 2 個の数字のならびを選べる
// -> 9 * 10^((K + 1)/2 - 1)
//
// K = 1 のときは先頭が 0 でも良い
//
// ここまでで、N 番目の回文数が何桁の何番目かまでわかる
//
// K ケタの回文数のうち M 番目の数は?
//
// K ケタのとき、上位 x = floor((k + 1) / 2) 桁は自由に決められる。
// ただし、最上位ケタだけは 0 になってはダメ (1 スタート)
// すなわち、K ケタの回文数のうち M (1-index) 番目の数は、上位 x ケタが 10^(x - 1) + M - 1 である

fn solve(n: usize) -> String {
    if n == 0 {
        return 1.to_string();
    }
    let mut n = n - 1; // 0 を除く
    let mut k = 1; // ケタ数
    loop {
        let t = 9 * usize::pow(10, (k + 1) / 2 - 1); // k ケタの回文数の数
        if n <= t {
            break; // ケタ数を確定
        }
        n -= t;
        k += 1;
    }
    let x = (k + 1) / 2;
    let y = usize::pow(10, x - 1) + n - 1;
    let s = y.to_string();
    let t: String = s.chars().rev().dropping((2 * x - k) as usize).collect();
    let ans: String = s.chars().chain(t.chars()).collect();
    ans
}

fn main() {
    input! {
        n: usize,
    }
    let ans = solve(n);
    println!("{}", ans);
}
