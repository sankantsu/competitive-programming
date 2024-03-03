#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    constexpr long inf = 1L<<60;
    vector<vector<long>> dist(n, vector<long>(n, inf));
    rep(i,n) {
        dist[i][i] = 0;
    }
    rep(i,m) {
        long u, v, w;
        cin >> u >> v >> w;
        u--; v--;
        dist[u][v] = w;
    }

    // WF
    rep(k,n) {
        rep(i,n) {
            rep(j,n) {
                dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j]);
            }
        }
    }

    vector<vector<long>> dp(1<<n, vector<long>(n, inf));
    rep(i,n) {
        dp[1 << i][i] = 0;
    }

    auto chmin = [](long& x, long y) {
        x = min(x, y);
    };
    for (long s = 1; s < (1 << n) - 1; s++) {
        for (long i = 0; i < n; i++) {
            long valid = (s >> i) & 0x1;
            if (!valid) continue;
            for (long k = 0; k < n; k++) {
                long visited = (s >> k) & 0x1;
                if (!visited) {
                    chmin(dp[s | (1 << k)][k], dp[s][i] + dist[i][k]);
                }
            }
        }
    }

    long ans = inf;
    rep(i,n) {
        ans = min(ans, dp[(1 << n) - 1][i]);
    }
    if (ans > inf/2) {
        cout << "No" << endl;
    }
    else {
        cout << ans << endl;
    }
}
