#include <bits/stdc++.h>
#include <atcoder/all>

using namespace std;
using namespace atcoder;
using mint = modint998244353;
// using mint = modint1000000007;

long l,r;

long gcd(long a, long b) {
    if (b == 0) return a;
    return gcd(b,a%b);
}

int main() {
    cin >> l >> r;
    long ans;
    for (long d = r-l; d >= 0; d--) {
        for (long x = l; x+d <= r; x++) {
            if (gcd(x,x+d) == 1) {
                ans = d;
                goto END;
            }
        }
    }
END:
    cout << ans << endl;
}
