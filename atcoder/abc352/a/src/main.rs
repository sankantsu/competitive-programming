fn main() {
  proconio::input! {
    n: i32,
    x: i32,
    y:i32,
    z:i32,
  }
  let b = if (x < z && z < y) || (y < z && z < x) {
    true
  } else {
    false
  };
  if b {
    println!("Yes")
  } else {
    println!("No")
  }
}
