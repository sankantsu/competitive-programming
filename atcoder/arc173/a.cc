#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long cnt_neq(long n) {  // n 以下の Neq number の個数
    string s = to_string(n);
    long m = s.size();
    /* cerr << "s: " << s << endl; */
    /* cerr << "m: " << m << endl; */
    // flag, digits (higher to lower)
    vector<vector<long>> dp(3, vector<long>(m, 0));
    dp[0][0] = 1;
    dp[1][0] = s[0] - '1';
    rep(i,m-1) {
        if (s[i] == s[i + 1]) {
            dp[0][i+1] = 0;
        }
        else {
            dp[0][i+1] = dp[0][i];
        }
        long cnt = s[i+1] - '0';
        if (s[i] < s[i+1]) cnt--;
        dp[1][i+1] += dp[0][i] * cnt;
        dp[1][i+1] += dp[1][i] * 9;
        dp[1][i+1] += 9;  // upper digits are all zero
        /* cerr << "i,dp[0],dp[1]: " << i+1 << " " << dp[0][i+1] << " " << dp[1][i+1] << endl; */
    }
    return dp[0][m-1] + dp[1][m-1];
}

int main() {
    long t;
    cin >> t;

    rep(_,t) {
        long k;
        cin >> k;

        long lb = 0;
        long ub = 1L<<50;
        while (ub - lb > 1) {
            long m = (lb + ub)/2;
            long cnt = cnt_neq(m);
            /* cerr << "m,cnt_neq: " << m << " " << cnt << endl; */
            bool check = cnt < k;
            if (check) {
                lb = m;
            }
            else {
                ub = m;
            }
        }
        cout << ub << endl;
    }
}
