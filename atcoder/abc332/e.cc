#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, d;
    cin >> n >> d;

    vector<long> w(n);
    rep(i,n) cin >> w[i];

    const long max_n = 15;
    const long max_w = 100000000L;
    const long max_sum = max_n * max_w;
    const long inf = max_sum*max_sum + 1;
    vector<vector<long>> dp(d, vector<long>(1<<n, inf));
    rep(s,1<<n) {
        long t = 0;
        rep(i,n) {
            if ((s>>i) & 1) t += w[i];
        }
        dp[0][s] = t*t;
    }
    rep(k,d-1) rep(s,1<<n) {
        int x = s;
        while (x > 0) {
            dp[k+1][s] = min(dp[k+1][s], dp[k][x] + dp[0][s-x]);
            x = (x-1)&s;
        }
    }

    long s = 0;
    rep(i,n) s += w[i];

    long v = dp[d-1][(1<<n)-1];
    long x = v*d - s*s;
    double ans = (double)x / (d*d);
    cout << fixed << setprecision(8) << ans << endl;
}
