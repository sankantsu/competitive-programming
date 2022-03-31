// TLE O(nw^2)

#include <iostream>
#include <algorithm>

using namespace std;

int w, n;
int l[501];
int r[501];
int v[501];

long dp[501][10001];

int main() {
    cin >> w >> n;
    for (int i = 1; i <= n; i++) {
        cin >> l[i] >> r[i] >> v[i];
    }
    for (int i = 0; i <= n; i++) {
        for (int j = 0; j <= w; j++) {
            dp[i][j] = -1;
        }
    }
    dp[0][0] = 0;
    for (int i = 1; i <= n; i++) {
        for (int j = 0; j <= w; j++) {
            for (int k = l[i]; k <= r[i]; k++) {
                if (j >= k && dp[i-1][j-k] != -1) {
                    dp[i][j] = max({dp[i][j],dp[i-1][j],v[i]+dp[i-1][j-k]});
                }
                else {
                    dp[i][j] = max(dp[i][j],dp[i-1][j]);
                }
            }
            /* cout << "i,j,dp[i][j]: " << i << " " << j << " " << dp[i][j] << endl; */
        }
    }
    cout << dp[n][w] << endl;
}
