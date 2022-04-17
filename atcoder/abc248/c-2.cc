#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

int n,m,K;
mint dp[100][3000]; // dp[i][j][k]: i番目まで使って和がkの数列の数

int main() {
    cin >> n >> m >> K;
    dp[0][0] = 1;
    rep(i,n) { // 1 to n
        for (int k = 0; k <= K; k++) {
            for (int j = 1; j <= m; j++) {
                if (k-j >= 0) {
                    dp[i+1][k] += dp[i][k-j];
                }
            }
        }
    }
    mint res = 0;
    rep(k,K+1) {
        res += dp[n][k];
    }
    cout << res.val() << endl;
}
