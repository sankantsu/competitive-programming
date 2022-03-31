#include <iostream>
#include <atcoder/modint>

using namespace std;
using namespace atcoder;

using mint = modint1000000007;

constexpr int max_k = 100000;

int k;

mint dp[max_k+1];

int main() {
    cin >> k;
    if (k % 9) {
        cout << 0 << endl;
        return 0;
    }

    dp[0] = 1;
    for (int i = 1; i <= k; i++) {
        for (int x = 1; x <= 9; x++) {
            if (i - x >= 0) {
                dp[i] += dp[i-x];
            }
        }
    }
    cout << dp[k].val() << endl;
}
