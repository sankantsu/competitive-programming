use ac_library::string::suffix_array;
use proconio::input;

fn main() {
    input! {
        s: String,
        q: usize,
        t: [String; q],
    }
    let n = s.len();
    let sa = suffix_array(&s);
    for t in t {
        let mut t2 = String::from(&t[0..t.len() - 1]);
        t2.push((t.chars().last().unwrap() as u8 + 1) as char);
        // binary search
        let binary_search = |t| {
            let mut ok: i64 = n as i64;
            let mut ng: i64 = -1;
            while ok - ng > 1 {
                let m = (ok + ng) / 2;
                if &s[sa[m as usize]..] >= t {
                    ok = m;
                } else {
                    ng = m;
                }
            }
            ok
        };
        let x = binary_search(&t);
        let y = binary_search(&t2);

        println!("{}", y - x);
    }
}
