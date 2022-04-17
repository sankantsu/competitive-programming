// D - Wall
// https://atcoder.jp/contests/abc079/tasks/abc079_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int h,w;
int c[10][10];
int a[200][200];

int dp[10][10];
int cost[10];

void warshall_floyd() {
    rep(i,10) rep(j,10) dp[i][j] = c[i][j];
    rep(k,10) rep(i,10) rep(j,10) {
        dp[i][j] = min(dp[i][j],dp[i][k]+dp[k][j]);
    }
    rep(i,10) {
        cost[i] = dp[i][1];
    }
}

int main() {
    cin >> h >> w;
    rep(i,10) rep(j,10) cin >> c[i][j];
    rep(i,h) rep(j,w) cin >> a[i][j];

    warshall_floyd();
    long res = 0;
    rep(i,h) rep(j,w) {
        if (a[i][j] != -1) res += cost[a[i][j]];
    }
    cout << res << endl;
}
