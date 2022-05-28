#include <iostream>

using namespace std;

const long mod = 998244353;

long n,m,k;

long dp[1010][5010];
long sum[1010][5010];

int main() {
    cin >> n >> m >> k;

    for (int j = 1; j <= m; j++) {
        sum[1][j+1] = j;
    }
    for (int i = 1; i < n; i++) {
        for (int j = 1; j <= m; j++) {
            if (k == 0) {
                dp[i+1][j] = sum[i][m+1]%mod;
            }
            else {
                long k1 = max(0L,j-k);
                long k2 = min(m+1,j+k);
                dp[i+1][j] = (sum[i][m+1] - sum[i][k2] + sum[i][k1+1] + 3*mod)%mod;
            }
            sum[i+1][j+1] = (sum[i+1][j] + dp[i+1][j])%mod;
        }
    }
    /* for (int i = 1; i <= n; i++) for (int j = 1; j <= m; j++) { */
    /*     cerr << "i,j,dp[i][j],sum[i][j+1]: " << i << " " << j << " " << dp[i][j] << " " << sum[i][j+1] << endl; */
    /* } */
    long res = 0;
    for (int j = 1; j <= m; j++) {
        res = (res + dp[n][j])%mod;
    }
    cout << res << endl;
}
