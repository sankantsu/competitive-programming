// D - トランプ挿入ソート
// https://atcoder.jp/contests/abc006/tasks/abc006_4
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int c[30000];

int dp[30000];

int main() {
    cin >> n;
    rep(i,n) cin >> c[i];

    const int inf = n+1;
    rep(i,n) dp[i] = inf;
    int res = 1;
    dp[0] = c[0];
    rep(i,n-1) {
        int k = distance(dp,lower_bound(dp,dp+n,c[i+1]));
        dp[k] = c[i+1];
        res = max(res,k+1);
    }
    cout << n-res << endl;
}
