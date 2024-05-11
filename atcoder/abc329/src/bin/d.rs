use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        mut a: [usize; m],
    }
    for x in &mut a {
        *x -= 1;
    }
    let mut cnt = vec![0; n];
    let mut ans = (0, 0);
    for &x in &a {
        cnt[x as usize] += 1;
        ans = ans.min((-cnt[x as usize], x));
        println!("{}", ans.1 + 1);
    }
}
