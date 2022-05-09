// AOJ2199 - Differential Pulse Code Modulation
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2199&lang=jp
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

bool solve() {
    long n,m;
    cin >> n >> m;
    if (n == 0 && m == 0) return false;
    vector<long> c(m);
    vector<long> x(n);
    rep(i,m) cin >> c[i];
    rep(i,n) cin >> x[i];

    const long inf = 1L<<50;
    vector<vector<long>> dp(n+1,vector<long>(256,inf));
    dp[0][128] = 0;
    rep(i,n) {
        rep(j,256) {
            for (auto diff : c) {
                long nj = min(255LL,max(0LL,j + diff));
                long penalty = (x[i]-nj)*(x[i]-nj);
                dp[i+1][nj] = min(dp[i+1][nj],dp[i][j]+penalty);
            }
        }
    }
    long res = inf;
    rep(j,256) res = min(res,dp[n][j]);
    cout << res << endl;
    return true;
}

int main() {
    while (solve());
}
