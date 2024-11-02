use proconio::input;
use proconio::marker::Chars;

// g -> g or p
// p -> p or t
// t -> t or g
//
// 前から順に勝てるときに勝つ手を出す貪欲で一見良さそうだが、以下のようなケースでダメ
//
// ggtpgtp
// pgtpgtp -> 1
// gpgtpgt -> 6
//
// 次に何を出せるかは最後の手だけに依存するから、最後の手が g,p,t それぞれについて全て考えるように
// dp すればよさそう?
//
// dp[i][s] = (i 番目まで見たときに最後の手を s にしたときの最大勝ち数)

fn to_num(c: char) -> usize {
    match c {
        // x に対して (x + 1) % 3 で勝てる
        'R' => 0,
        'P' => 1,
        'S' => 2,
        _ => panic!("Unexpected char!"),
    }
}

fn main() {
    input! {
        n: usize,
        s: Chars,
    }
    let inf = 100000;
    let t = s.iter().map(|c| to_num(*c)).collect::<Vec<_>>();
    let mut dp = vec![vec![-inf; 3]; n];
    let win = |x| (x + 1) % 3;
    dp[0][t[0]] = 0;
    dp[0][win(t[0])] = 1;
    for i in 1..n {
        dp[i][t[i]] = std::cmp::max(dp[i - 1][win(t[i])], dp[i - 1][win(win(t[i]))]);
        dp[i][win(t[i])] = 1 + std::cmp::max(dp[i - 1][t[i]], dp[i - 1][win(win(t[i]))]);
    }
    let ans = dp[n - 1].iter().max().unwrap();
    println!("{ans}");
}
