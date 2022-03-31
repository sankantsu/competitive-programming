#include <iostream>
#include <atcoder/all>
#include <deque>
#include <cmath>

using namespace std;
using namespace atcoder;

using mint = modint998244353;

void solve(int n) {
    mint dp[2][10];
    for (int i = 1; i <= 9; i++) {
        dp[0][i] = 1;
    }
    for (int k = 2; k <= n; k++) {
        dp[(k+1)%2][1] = dp[k%2][1] + dp[k%2][2];
        dp[(k+1)%2][9] = dp[k%2][8] + dp[k%2][9];
        for (int i = 2; i <= 8; i++) {
            dp[(k+1)%2][i] = dp[k%2][i-1] + dp[k%2][i] + dp[k%2][i+1];
        }
    }
    mint ans;
    for (int i = 1; i <=9; i++) {
        ans += dp[(n+1)%2][i];
    }
    cout << ans.val() << endl;
}

int main() {
    int n;
    cin >> n;

    solve(n);
}
