// AC

#include <iostream>
#include <vector>
#include <string>

using namespace std;

int n, s;
int a[101];
int b[101];

string dp[2][100001];

void print(const string &ans) {
    if (ans == "") {
        cout << "Impossible" << endl;
        return;
    }
    cout << ans.substr(1) << endl;
}

int main() {
    cin >> n >> s;
    for (int i = 1; i <= n; i++) cin >> a[i] >> b[i];

    for (int i = 0; i < 2; i++) {
        for (int j = 0; j <= s; j++) {
            dp[i][j] = "";
        }
    }
    dp[0][0].push_back('S'); // start mark
    for (int i = 1; i <= n; i++) {
        for (int j = 0; j <= s; j++) {
            if (j >= a[i] && dp[1-(i&1)][j-a[i]].size()) {
                dp[i&1][j] = dp[1-(i&1)][j-a[i]];
                dp[i&1][j].push_back('A');
            }
            if (j >= b[i] && dp[1-(i&1)][j-b[i]].size()) {
                dp[i&1][j] = dp[1-(i&1)][j-b[i]];
                dp[i&1][j].push_back('B');
            }
            /* cout << "i,j: " << i << " " << j << " " << dp[i][j] << endl; */
        }
        for (int j = 0; j <= s; j++) {
            auto &str = dp[1-(i&1)][j];
            str.erase(str.begin(),str.end());
        }
    }
    print(dp[n&1][s]);
}
