// Longest Increasing Subsequence (LIS)
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
long a[100000];

long dp[100000]; // 長さi+1のLISで最も右端の数字が小さいときの右端の数字

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    const long inf = 1L<<60;
    rep(i,n) dp[i] = inf;

    dp[0] = a[0];
    long res = 1;
    rep(i,n-1) {
        auto k = distance(dp,lower_bound(dp,dp+n,a[i+1]));
        dp[k] = a[i+1];
        res = max(res,k+1);
    }
    cout << res << endl;
}
