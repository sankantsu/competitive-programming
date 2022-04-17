// knapsac DP O(NM) AC

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n, q;
int a[21];
int m[201];
bool ans[201];

bool dp[21][2001];

int main() {
    cin >> n;
    for (int i = 1; i <= n; i++) cin >> a[i];
    cin >> q;
    for (int i = 0; i < q; i++) cin >> m[i];

    for (int j = 0; j <= n; j++) {
        for (int k = 0; k <= 2000; k++) {
            dp[j][k] = false;
        }
    }
    dp[0][0] = true;
    for (int j = 1; j <= n; j++) {
        for (int k = 0; k <= 2000; k++) {
            dp[j][k] = dp[j-1][k];
            if (k >= a[j] && dp[j-1][k-a[j]]) {
                /* cout << "j,k: " << j << " " << k << endl; */
                dp[j][k] = true;
            }
        }
    }

    for (int i = 0; i < q; i++) {
        int mi = m[i];
        ans[i] = dp[n][mi];
    }
    for (int i = 0; i < q; i++) {
        if (ans[i]) {
            cout << "yes" << endl;
        }
        else {
            cout << "no" << endl;
        }
    }
}
