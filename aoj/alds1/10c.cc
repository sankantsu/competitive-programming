// Longest Common Subsequence (最長共通部分列)
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int q;

int main() {
    cin >> q;
    rep(i,q) {
        string s;
        string t;
        cin >> s;
        cin >> t;
        const long n = s.size();
        const long m = t.size();
        vector<vector<int>> dp(n+1,vector<int>(m+1,0));
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= m; j++) {
                dp[i][j] = max(dp[i-1][j],dp[i][j-1]);
                if (s[i-1] == t[j-1]) {
                    dp[i][j] = max(dp[i][j],1+dp[i-1][j-1]);
                }
            }
        }
        cout << dp[n][m] << endl;
    }
}
