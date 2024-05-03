#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// dp[k]: 現在の数字が k であるときの金額の期待値
// dp[k] = min(dp[k/a] + x, Σ_i (dp[k/i] + y)/6)
// dp[0] = 0
//
// サイコロ振る場合
// dp[k] = ((dp[k] + y) + (dp[k/2] + y) + ... + (dp[k/6] + y))/6
//       = (dp[k] + ... dp[k/6])/6 + y
// 5/6 dp[k] = (dp[k/2] + ... dp[k/6])/6 + y
// dp[k] = (dp[k/2] + .. + dp[k/6])/5 + 6/5 y

long n, A, x, y;
map<long, double> dp;

double expect(long k) {
    if (dp.count(k)) {
        return dp[k];
    }
    double a = expect(k/A) + x;
    double b = 0;
    for (long i = 2; i <= 6; i++) {
        b += expect(k/i);
    }
    b = (b + 6*y) / 5;
    double e = min(a, b);
    dp[k] = e;
    return e;
}

int main() {
    dp[0] = 0;
    cin >> n >> A >> x >> y;

    double ans = expect(n);
    cout << fixed << setprecision(8) << ans << endl;
}
