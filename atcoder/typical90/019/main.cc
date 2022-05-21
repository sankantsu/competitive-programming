// 019 - Pick Two
// https://atcoder.jp/contests/typical90/tasks/typical90_s
#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 200;
const long inf = 1L<<60;

int n;
long a[2*max_n+10];

long dp[2*max_n+10][2*max_n+10];

long rec(int l, int r) {
    /* cerr << "rec: " << l << " " << r << endl; */
    assert((r-l)%2 == 0);
    if (dp[l][r] != inf) {
        return dp[l][r];
    }
    if (r-l == 2) {
        long cost = abs(a[l] - a[l+1]);
        dp[l][r] = cost;
        return dp[l][r];
    }
    else {
        for (int k = l+2; k <= r-2; k += 2) {
            dp[l][r] = min(dp[l][r],rec(l,k)+rec(k,r));
        }
        dp[l][r] = min(dp[l][r],rec(l+1,r-1)+abs(a[r-1]-a[l]));
        return dp[l][r];
    }
}

int main() {
    cin >> n;
    rep(i,2*n) cin >> a[i];

    rep(i,2*n+1) rep(j,2*n+1) dp[i][j] = inf;
    long res = rec(0,2*n);
    cout << res << endl;
}
