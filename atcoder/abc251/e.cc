#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n;
long a[300010];

long dp[2][300010];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    const long inf = 1L<<60;
    rep(i,n) {
        dp[0][i] = inf;
        dp[1][i] = inf;
    }

    dp[0][0] = a[n-1]; // 操作Nを使う (すでに N-1 番目の動物は餌をもらっている)
    dp[1][0] = a[0];   // 操作Nを使わない
    dp[1][1] = a[0];
    for (int i = 1; i < n; i++) {
        dp[0][i] = min(dp[0][i],dp[0][i-1]+a[i]);
        dp[1][i] = min(dp[1][i],dp[1][i-1]+a[i]);
        if (i < n-1) {
            dp[0][i+1] = min(dp[0][i+1],dp[0][i-1]+a[i]);
            dp[1][i+1] = min(dp[1][i+1],dp[1][i-1]+a[i]);
        }
    }
    cout << min(dp[0][n-2],dp[1][n-1]) << endl;
}
