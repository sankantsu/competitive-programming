#include <iostream>
#include <vector>
#include <atcoder/modint.hpp>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    using T = mint;
    vector<T> pow2(n+1);
    pow2[0] = 1;
    rep(i,n) pow2[i+1] = pow2[i]*2;

    vector<vector<T>> dp(n+1, vector<T>(n));
    dp[1][0] = 1;

    for (int i = 2; i <= n; i++) {
        T s = 0;
        for (int j = 0; j < i - 1; j++) {
            s += dp[i-1][j] / pow2[i - 1 - j];
        }
        T x = pow2[i - 1]*s / (pow2[i] - 1);

        /* cerr << "s,x: " << s << " " << x << endl; */

        dp[i][0] = x;
        for (int j = 0; j < i - 1; j++) {
            dp[i][j+1] = (dp[i][j] + dp[i-1][j])/2;
        }
    }

    rep(i,n) cout << dp[n][i].val() << " "; cout << endl;
}
