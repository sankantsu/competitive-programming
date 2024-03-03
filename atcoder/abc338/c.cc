#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr long inf = 1L<<60;

int main() {
    long n;
    cin >> n;

    vector<long> q(n), a(n), b(n);
    rep(i,n) cin >> q[i];
    rep(i,n) cin >> a[i];
    rep(i,n) cin >> b[i];

    auto max_y = [&](long x) {
        long y = inf;
        rep(i,n) {
            if (b[i] == 0) continue;
            y = min(y, (q[i] - a[i]*x) / b[i]);
        }
        return y;
    };

    long max_x = inf;
    rep(i,n) {
        if (a[i] == 0) continue;
        max_x = min(max_x, q[i]/a[i]);
    }

    long ans = -1;
    rep(x,max_x+1) {
        long y = max_y(x);
        ans = max(ans, (long)(x + y));
    }
    cout << ans << endl;
}
