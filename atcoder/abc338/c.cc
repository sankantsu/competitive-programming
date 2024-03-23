#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> q(n), a(n), b(n);
    rep(i,n) cin >> q[i];
    rep(i,n) cin >> a[i];
    rep(i,n) cin >> b[i];

    constexpr long inf = 1L<<60;
    long k = inf;
    rep(i,n) {
        if (a[i] == 0) continue;
        k = min(k, q[i]/a[i]);
    }

    long ans = 0;
    rep(x,k+1) {
        long y = inf;
        rep(i,n) {
            if (q[i] - a[i]*x < 0) {
                y = inf;
                break;
            }
            if (b[i] == 0) continue;
            y = min(y, (q[i] - a[i]*x)/b[i]);
        }
        /* cerr << "x,y: " << x << " " << y << endl; */
        ans = max(ans, x + y);
    }
    cout << ans << endl;
}
