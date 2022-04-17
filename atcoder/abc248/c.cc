#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

int n,m,K;
mint dp[100][100][3000]; // dp[i][j][k]: i番目まで使って最後の数がjかつ和がkの数列の数

int main() {
    cin >> n >> m >> K;
    dp[0][1][0] = 1;
    rep(i,n) { // 1 to n
        for (int j = 1; j <= m; j++) {
            for (int k = 0; k <= K; k++) {
                if (k-j < 0) continue;
                for (int l = 1; l <= j; l++) {
                        dp[i+1][j][k] += dp[i][l][k-j];
                }
                cout << i+1 << " " << j << " " << k << " " << dp[i+1][j][k].val() << endl;
            }
        }
    }
    mint res = 0;
    for (int j = 1; j <= m; j++) {
        rep(k,K+1) {
            res += dp[n][j][k];
        }
    }
    cout << res.val() << endl;
}
