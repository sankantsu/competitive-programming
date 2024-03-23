#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <atcoder/modint.hpp>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

int main() {
    long n;
    cin >> n;

    /* long m = 1 + sqrt(n); */
    long m = 400;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    vector<mint> dp(n);
    vector<vector<mint>> s(m, vector<mint>(n));
    dp[0] = 1;
    rep(i,n) {
        for (long j = 0; j < m; j++) {
            dp[i] += s[j][i];
            if (i + j < n) s[j][i + j] += s[j][i];
        }
        if (a[i] >= m) {
            for (long j = 1; i + j*a[i] < n; j++) {
                dp[i + j*a[i]] += dp[i];
            }
        }
        else {
            if (i + a[i] < n) s[a[i]][i + a[i]] += dp[i];
        }
    }

    mint ans = 0;
    rep(i,n) {
        ans += dp[i];
    }
    cout << ans.val() << endl;
}
