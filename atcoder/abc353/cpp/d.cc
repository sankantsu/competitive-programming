#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <atcoder/modint.hpp>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int ndigit(int x) {
    int cnt = 1;
    while (x >= 10) {
        x /= 10;
        cnt++;
    }
    return cnt;
}

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    // dp[i][j]: i 番目以降に j 桁の数字がいくつあるか
    const int max_digit = 11;
    vector<vector<long>> dp(n+1, vector<long>(max_digit));
    rep(i,n) {
        int m = ndigit(a[n-1-i]);
        rep(j,max_digit) {
            if (j == m) {
                dp[n-1-i][j] = dp[n-i][j] + 1;
            }
            else {
                dp[n-1-i][j] = dp[n-i][j];
            }
        }
    }

    mint s = 0;
    rep(i,n) {
        s += mint(a[i]) * i;
        rep(j,max_digit) {
            long p = 1;
            rep(k,j) p *= 10;
            s += mint(a[i]) * p * dp[i+1][j];
        }
    }
    cout << s.val() << endl;
}
