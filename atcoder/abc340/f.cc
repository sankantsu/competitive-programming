#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long ext_gcd(long a, long b, long& x, long& y) {
    if (b == 0) {
        x = 1; y = 0;
        return a;
    }
    long d = ext_gcd(b, a%b, y, x);
    y -= (a/b)*x;
    return d;
}

int main() {
    long x,y;
    cin >> x >> y;

    long a, b;
    long d = ext_gcd(x, y, b, a);
    if (abs(d) > 2) {
        cout << -1 << endl;
    }
    else {
        cout << a*(2/d) << " " << -b*(2/d) << endl;
    }
}
