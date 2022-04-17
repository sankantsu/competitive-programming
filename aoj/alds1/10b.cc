#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
long row[101];
long col[101];

long dp[101][101];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> row[i] >> col[i];

    const long inf = 10000000;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            dp[i][j] = inf;
        }
        dp[i][i] = 0;
    }
    for (int d = 1; d < n; d++) {
        for (int l = 0; l+d < n; l++) {
            int r = l+d;
            for (int k = 0; k < d; k++) {
                dp[l][r] = min(dp[l][r],
                    dp[l][l+k]+dp[l+k+1][r]
                    +row[l]*col[l+k]*col[r]);
            }
        }
    }
    /* for (int i = 0; i < n; i++) { */
    /*     for (int j = 0; j < n; j++) { */
    /*         if (j < i) { */
    /*             cout << "* "; */
    /*         } */
    /*         else { */
    /*             cout << dp[i][j] << " "; */
    /*         } */
    /*     } */
    /*     cout << endl; */
    /* } */
    cout << dp[0][n-1] << endl;
}
