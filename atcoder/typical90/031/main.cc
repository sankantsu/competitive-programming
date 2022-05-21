// 031 - VS AtCoder
// https://atcoder.jp/contests/typical90/tasks/typical90_ae
#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 100000;

int n;
int w[max_n];
int b[max_n];

int dp[51][2000];

void rec(int w, int b) {
    if (dp[w][b] != -1) {
        return;
    }
    if (w == 0 && b == 1) {
        dp[w][b] = 0;
        return;
    }
    vector<bool> v(3000,true);
    if (w >= 1) {
        rec(w-1,b+w);
        v[dp[w-1][b+w]] = false;
    }
    for (int k = 1; k <= b/2; k++) {
        rec(w,b-k);
        v[dp[w][b-k]] = false;
    }
    int i = 0;
    while (true) {
        if (v[i]) {
            dp[w][b] = i;
            return;
        }
        i++;
    }
}

int main() {
    cin >> n;
    rep(i,n) cin >> w[i];
    rep(i,n) cin >> b[i];

    rep(i,51) rep(j,2000) dp[i][j] = -1;
    rep(i,51) rep(j,51) {
        rec(i,j);
    }

    int res = 0;
    rep(i,n) {
        res ^= dp[w[i]][b[i]];
    }
    if (res) {
        cout << "First" << endl;
    }
    else {
        cout << "Second" << endl;
    }
}
