#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;

long dp[100];

int main() {
    cin >> n;
    dp[0] = 1;
    dp[1] = 1;
    for (int i = 2; i <= n; i++) {
        dp[i] = dp[i-1] + dp[i-2];
    }
    cout << dp[n] << endl;
}
