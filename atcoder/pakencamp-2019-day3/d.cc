// D - パ研軍旗
// https://atcoder.jp/contests/pakencamp-2019-day3/tasks/pakencamp_2019_day3_d
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int row = 5;
const int C = 3;
const int inf = 10000;

const char color[] = "RBW#";

int n;
int dp[1000][5]; // i列目までの塗り方でi列目が色jであるときの塗り替えるマスの数の最小値

int main() {
    cin >> n;
    vector<string> s(row);
    rep(i,row) cin >> s[i];

    rep(i,n) rep(j,C) dp[i+1][j] = inf;
    rep(i,n) {
        rep(j,C) {
            int cnt = 0;
            rep(r,row) {
                if (s[r][i] != color[j]) cnt++;
            }
            rep(k,C) {
                if (j == k) continue;
                dp[i+1][j] = min(dp[i+1][j],dp[i][k]+cnt);
            }
        }
    }
    int res = inf;
    rep(j,C) {
        res = min(res,dp[n][j]);
    }
    cout << res << endl;
}
