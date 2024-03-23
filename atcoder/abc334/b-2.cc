#include <iostream>

using namespace std;

long floor_rational(long a, long b) {
    long r = (a % b + b) % b;  // a ÷ b のあまり (0 <= r < b)
    return (a - r)/b;
}

int main() {
    long a, m, l, r;
    cin >> a >> m >> l >> r;

    cout << floor_rational(r - a, m) - floor_rational(l - 1 - a, m) << endl;
}
