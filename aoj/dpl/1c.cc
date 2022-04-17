#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n,w;
int val[200];
int wei[200];

int dp[200][10001];

int main() {
    cin >> n >> w;
    for (int i = 1; i <= n; i++) {
        cin >> val[i] >> wei[i];
    }
    for (int i = 1; i <= n; i++) {
        for (int j = 0; j <= w; j++) {
            dp[i][j] = dp[i-1][j];
            if (j >= wei[i]) {
                dp[i][j] = max(dp[i][j],dp[i][j-wei[i]]+val[i]);
            }
        }
    }
    cout << dp[n][w] << endl;
}
