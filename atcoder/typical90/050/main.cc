#include <iostream>
#include <atcoder/modint>

using namespace std;
using namespace atcoder;

using mint = modint1000000007;

constexpr int max_n = 100000;

int n,l;

mint dp[max_n+1];

int main() {
    cin >> n >> l;

    dp[0] = 1;
    for (int i = 1; i <= n; i++) {
        mint tmp = 0;
        if (i - l >= 0) {
            tmp = dp[i-l];
        }
        dp[i] = dp[i-1] + tmp;
    }

    cout << dp[n].val() << endl;
}
