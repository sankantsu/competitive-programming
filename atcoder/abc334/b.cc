#include <iostream>

using namespace std;

int main() {
    long a, m, l, r;
    cin >> a >> m >> l >> r;

    long k1 = (a < l) ? (l - 1 - a)/m : (a - l)/m*(-1) - 1;
    long k2 = (a <= r) ? (r - a)/m : (a - r - 1)/m*(-1) - 1;
    cout << k2 - k1 << endl;
}
