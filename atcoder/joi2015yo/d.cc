// D - シルクロード
// https://atcoder.jp/contests/joi2015yo/tasks/joi2015yo_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
long d[1001];
long c[1001];

long dp[1001][1001]; // i日目に都市jにいるような移動方法のうち最も疲労度の小さいもの

int main() {
    cin >> n >> m;
    rep(i,n) cin >> d[i];
    rep(i,m) cin >> c[i];

    const long inf = 1L<<50;
    rep(i,m+1) rep(j,n+1) dp[i][j] = inf;
    dp[0][0] = 0;
    rep(i,m) rep(j,n+1) {
        dp[i+1][j] = min(dp[i+1][j],dp[i][j]);
        if (j < n) {
            dp[i+1][j+1] = min(dp[i+1][j+1],dp[i][j]+c[i]*d[j]);
        }
    }
    /* rep(i,m+1) { */
    /*     rep(j,n+1) cout << dp[i][j] << " "; */
    /*     cout << endl; */
    /* } */
    cout << dp[m][n] << endl;
}
