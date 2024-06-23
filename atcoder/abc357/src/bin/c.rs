use proconio::input;

fn solve(n: usize) -> Vec<String> {
    if n == 0 {
        vec![String::from('#')]
    } else {
        let pv = solve(n - 1);
        let sep = ".".repeat(pv[0].len());
        let a = pv.iter().map(|row| row.repeat(3)).collect::<Vec<_>>();
        let b = pv.iter().map(|row| format!("{}{}{}", row, sep, row)).collect::<Vec<_>>();
        a.iter().chain(b.iter()).chain(a.iter()).cloned().collect()
    }
}

fn main() {
    input! {
        n: usize,
    }
    let ans = solve(n);
    println!("{}", ans.join("\n"));
}
