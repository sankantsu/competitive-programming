#include <iostream>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

constexpr int max_n = 100;

int n;
int a[max_n+1][6];

mint dp[max_n+1];

int main() {
    cin >> n;
    for (int i = 1; i <= n; i++) {
        for (int k = 0; k < 6; k++) {
            cin >> a[i][k];
        }
    }

    dp[0] = 1;
    for (int i = 1; i <= n; i++) {
        for (int k = 0; k < 6; k++) {
            dp[i] += a[i][k]*dp[i-1];
        }
    }

    cout << dp[n].val() << endl;
}
