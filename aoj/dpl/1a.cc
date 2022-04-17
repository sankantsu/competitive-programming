// coin changing problem
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
int c[30];

int dp[30][60000];

int main() {
    cin >> n >> m;
    rep(i,m) cin >> c[i+1];

    const int inf = 1<<30;
    rep(i,m+1) {
        rep(j,n+1) {
            dp[i][j] = inf;
        }
        dp[i][0] = 0;
    }
    for (int i = 1; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            dp[i][j] = dp[i-1][j];
            if (j >= c[i]) {
                dp[i][j] = min(dp[i][j],1+dp[i][j-c[i]]);
            }
        }
    }
    /* rep(i,m) { */
    /*     rep(j,n+1) { */
    /*         if (dp[i][j] != inf) { */
    /*             cout << "i,j,dp[i][j]: " << i << " " << j << " " << dp[i][j] << endl; */
    /*         } */
    /*     } */
    /* } */
    cout << dp[m][n] << endl;
}
