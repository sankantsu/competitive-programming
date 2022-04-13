#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
string s;

int dp[50000][10]; // dp[i][j]: i以降で最初に出現する文字jの位置

void make_table() {
    rep(i,n+1) {
        rep(j,10) {
            dp[i][j] = -1;
        }
    }
    for (int i = n-1; i >= 0; i--) {
        rep(j,10) {
            dp[i][j] = dp[i+1][j];
        }
        int j = s[i]-'0';
        dp[i][j] = i;
    }
}

// i,j,k を並べたものを暗証番号として使えるか?
bool check(int i, int j, int k) {
    int cur = 0;
    cur = dp[cur][i];
    if (cur == -1) {
        return false;
    }
    cur = dp[cur+1][j];
    if (cur == -1) {
        return false;
    }
    cur = dp[cur+1][k];
    if (cur == -1) {
        return false;
    }
    return true;
}

int main() {
    cin >> n;
    cin >> s;
    make_table();
    int cnt = 0;
    rep(i,10) {
        rep(j,10) {
            rep(k,10) {
                if (check(i,j,k)) {
                    cnt++;
                }
            }
        }
    }
    cout << cnt << endl;
}
