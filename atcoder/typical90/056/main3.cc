#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int n, s;
int a[101];
int b[101];

bool dp[101][100001];

void print(bool ans) {
    if (!ans) {
        cout << "Impossible" << endl;
        return;
    }
    int p = s;
    string str;
    for (int i = n; i >= 1; i--) {
        if (p-a[i] >= 0 && dp[i-1][p-a[i]]) {
            str.push_back('A');
            p -= a[i];
        }
        else if (p-b[i] >= 0 && dp[i-1][p-b[i]]) {
            str.push_back('B');
            p -= b[i];
        }
        else {
            throw;
        }
    }
    reverse(str.begin(),str.end());
    cout << str << endl;
}

int main() {
    cin >> n >> s;
    for (int i = 1; i <= n; i++) cin >> a[i] >> b[i];

    for (int i = 0; i <= n; i++) {
        for (int j = 0; j <= s; j++) {
            dp[i][j] = false;
        }
    }
    dp[0][0] = true;
    for (int i = 1; i <= n; i++) {
        for (int j = 0; j <= s; j++) {
            if (j >= a[i] && dp[i-1][j-a[i]]) {
                dp[i][j] = true;
            }
            if (j >= b[i] && dp[i-1][j-b[i]]) {
                dp[i][j] = true;
            }
            /* cout << "i,j: " << i << " " << j << " " << dp[i][j] << endl; */
        }
    }
    print(dp[n][s]);
}
