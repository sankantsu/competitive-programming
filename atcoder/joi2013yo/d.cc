// D - 暑い日々
// https://atcoder.jp/contests/joi2013yo/tasks/joi2013yo_d
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int d,n;
int t[300];
int a[300];
int b[300];
int c[300];

int dp[300][300]; // i日目に服jを着る計画のうちもっとも高いスコア (そのような計画がなければ-1)

int main() {
    cin >> d >> n;
    rep(i,d) cin >> t[i];
    rep(i,n) {
        cin >> a[i] >> b[i] >> c[i];
    }
    rep(j,n) {
        if (a[j] <= t[0] && t[0] <= b[j]) {
            dp[0][j] = 0;
        }
        else {
            dp[0][j] = -1;
        }
    }
    rep(i,d-1) {
        rep(j,n) {
            dp[i+1][j] = -1;
            if (a[j] <= t[i+1] && t[i+1] <= b[j]) {
                rep(k,n) {
                    if (dp[i][k] >= 0) {
                        int diff = abs(c[j]-c[k]);
                        dp[i+1][j] = max(dp[i+1][j],dp[i][k]+diff);
                    }
                }
            }
        }
    }
    int res = -1;
    rep(j,n) {
        res = max(res,dp[d-1][j]);
    }
    cout << res << endl;
}
