// D - A First Grader
// https://atcoder.jp/contests/joi2011yo/tasks/joi2011yo_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int a[200];

long dp[200][30];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    dp[1][a[0]] = 1;
    for (int i = 1; i < n-1; i++) {
        rep(j,21) {
            if (j >= a[i]) {
                dp[i+1][j] += dp[i][j-a[i]];
            }
            if (j+a[i] <= 20) {
                dp[i+1][j] += dp[i][j+a[i]];
            }
        }
    }
    cout << dp[n-1][a[n-1]] << endl;
}
