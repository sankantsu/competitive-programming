#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, s;
    cin >> n >> s;

    vector<long> a(n);
    rep(i,n) cin >> a[i];
    /* a[n-1] = s + 1;  // 場合分けを減らす hack */

    vector<long> sum(n+1);
    rep(i,n) sum[i+1] = sum[i] + a[i];

    vector<long> ub(n);
    rep(i,n) {
        auto it = upper_bound(sum.begin(), sum.end(), sum[i] + s);
        ub[i] = distance(sum.begin(), it) - 1;
        /* cerr << "i,ub[i]: " << i << " " << ub[i] << endl; */
    }

    vector<long> dp(n+1);
    dp[n] = 0;
    rep(i,n) {
        long j = n - 1 - i;
        dp[j] = dp[ub[j]] + (n - j);
    }

    long ans = 0;
    rep(i,n) ans += dp[i];

    cout << ans << endl;
}
