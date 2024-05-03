#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>
#include <algorithm>
#include <atcoder/segtree.hpp>

using atcoder::segtree;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

double op(double l, double r) {
    return min(l, r);
}

double e() {
    constexpr double h = 2e9;
    constexpr double max_n = 200000;
    return 2*sqrt(2)*h*max_n;
}

int main() {
    long n, k;
    cin >> n >> k;

    long sx, sy;
    cin >> sx >> sy;

    vector<long> x(n), y(n);
    rep(i,n) cin >> x[i] >> y[i];

    vector<double> dist(n-1);
    rep(i,n-1) {
        long dx = x[i+1] - x[i];
        long dy = y[i+1] - y[i];
        dist[i] = sqrt(dx*dx + dy*dy);
    }

    vector<double> dist2(n-1);
    rep(i,n-1) {
        long dx1 = sx - x[i];
        long dy1 = sy - y[i];
        long dx2 = x[i+1] - sx;
        long dy2 = y[i+1] - sy;
        dist2[i] = sqrt(dx1*dx1 + dy1*dy1) + sqrt(dx2*dx2 + dy2*dy2);
    }

    vector<double> diff(n-1);
    rep(i,n-1) {
        diff[i] = dist2[i] - dist[i];
    }

    segtree<double, op, e> dp(n-1);
    rep(i,n-1) {
        if (i < k) dp.set(i, diff[i]);
        else {
            double d = dp.prod(i - k, i) + diff[i];
            dp.set(i, d);
        }
    }

    double ans = 0;
    {
        long dx = x[0] - sx, dy = y[0] - sy;
        long dx2 = sx - x[n-1], dy2 = sy - y[n-1];
        double d1 = sqrt(dx*dx + dy*dy);
        double d2 = sqrt(dx2*dx2 + dy2*dy2);
        ans = d1 + d2;
    }
    rep(i,n-1) {
        ans += dist[i];
    }
    if (k < n) ans += dp.prod(n - 1 - k, n - 1);
    cout << fixed << setprecision(8) << ans << endl;
}
