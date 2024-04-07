#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

long dp[202020][2][2];  // i, last, cnt

int main() {
    long n;
    cin >> n;

    string s;
    cin >> s;

    vector<long> c(n);
    rep(i,n) cin >> c[i];

    const long inf = 1L<<60;
    rep(i,n+1) rep(j,2) rep(k,2) dp[i][j][k] = inf;
    dp[1][0][0] = (s[0] == '0') ? 0 : c[0];
    dp[1][1][0] = (s[0] == '1') ? 0 : c[0];
    for (long i = 1; i < n; i++) rep(j,2) rep(k,2) {
        rep(nj, 2) {
            long nk = k + (j == nj);
            long cost = (s[i] == '0' + nj) ? 0 : c[i];
            if (nk >= 2) continue;
            /* cerr << "i,j,k,ni,nj,nk,cost: " << i << " " << j << " " << k << " " << i+1 << " " << nj << " " << nk << " " << cost << endl; */
            dp[i+1][nj][nk] = min(dp[i+1][nj][nk], dp[i][j][k] + cost);
        }
    }
    /* for (long i = 1; i <= n; i++) rep(j,2) rep(k,2) cerr << "i,j,k,dp: " << i << " " << j << " " << k << " " << dp[i][j][k] << endl; */

    long ans = inf;
    rep(j,2) ans = min(ans, dp[n][j][1]);
    cout << ans << endl;
}
