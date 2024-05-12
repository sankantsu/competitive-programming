use proconio::*;
use ac_library::ModInt998244353 as m;
fn main() {
    input!{n:usize,a:[i64;n]}
    let mut r=m::new(0);
    let mut p=r;
    for(i,x)in a.iter().enumerate().rev(){
        r+=m::new(*x)*(p+i);
        p+=m::new(10).pow(x.to_string().len()as u64);
    }
    println!("{r}");
}
