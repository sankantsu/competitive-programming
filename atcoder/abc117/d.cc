#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    vector<vector<long>> count_digits(61, vector<long>(2));
    rep(i,61) {
        rep(j,n) {
            count_digits[i][1] += (a[j] >> i) & 0x1;
        }
        count_digits[i][0] = n - count_digits[i][1];
    }

    vector<vector<long>> dp(62, vector<long>(2));
    for (long i = 60; i >= 0; i--) {
        long x = (k >> i) & 0x1;
        long y = max(count_digits[i][0], count_digits[i][1]);
        dp[i][1] = dp[i+1][1] + (1L << i) * count_digits[i][1-x];
        if (dp[i+1][0] > 0) {
            dp[i][0] = dp[i+1][0] + (1L << i) * y;
        }
        if (x == 1) {
            dp[i][0] = max(dp[i][0], dp[i+1][1] + (1L << i) * count_digits[i][1]);
        }
        /* cerr << "i, dp[i][1], dp[i][0]: " << i << " " << dp[i][1] << " " << dp[i][0] << endl; */
    }
    cout << max(dp[0][0], dp[0][1]) << endl;
}
