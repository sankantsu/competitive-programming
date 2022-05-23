#include <iostream>

using namespace std;

const int mod = 998244353;

int n;
int p[510];
long dp[510][510][2];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> p[i];
    }
    for (int l = 0; l < n; l++) {
        dp[l][l+1][0] = 1;
        dp[l][l+1][1] = 1;
    }
    for (int d = 2; d <= n; d++) {
        for (int l = 0; l <= n-d; l++) {
            int r = l+d;
            for (int k = l+1; k < r; k++) {
                long s = dp[l][k][1]*dp[k][r][0];
                dp[l][r][0] = (dp[l][r][0]+s)%mod;
                if (p[r] > p[k]) dp[l][r][1] = (dp[l][r][1]+s)%mod;
            }
        }
    }
    cout << dp[0][n][0] << endl;
}
