#include <iostream>

using namespace std;

int dp[50][50];

int main() {
    int n;
    cin >> n;
    dp[0][0] = 1;
    for (int i = 1; i < n; i++) {
        for (int j = 0; j <= i; j++) {
            if (j == 0 || j == i) {
                dp[i][j] = 1;
            }
            else {
                dp[i][j] = dp[i-1][j-1] + dp[i-1][j];
            }
        }
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j <= i; j++) {
            cout << dp[i][j] << " ";
        }
        cout << endl;
    }
}
