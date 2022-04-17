#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n, w;
int value[200];
int weight[200];

long dp[200][10001];

int main() {
    cin >> n >> w;
    for (int i = 1; i <= n; i++) cin >> value[i] >> weight[i];

    for (int i = 1; i <= n; i++) {
        for (int j = 0; j <= w; j++) {
            dp[i][j] = dp[i-1][j];
            if (j >= weight[i]) {
                dp[i][j] = max(dp[i][j],dp[i-1][j-weight[i]]+value[i]);
            }
        }
    }
    cout << dp[n][w] << endl;
}
