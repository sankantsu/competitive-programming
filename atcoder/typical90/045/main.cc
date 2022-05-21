// 045 - Simple Grouping
// https://atcoder.jp/contests/typical90/tasks/typical90_as
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
long x[20];
long y[20];

long dist_two[20][20];
long dist_group[1<<20];

long dp[1<<20][20];

void init() {
    rep(i,n) rep(j,n) {
        dist_two[i][j] = (x[i]-x[j])*(x[i]-x[j]) + (y[i]-y[j])*(y[i]-y[j]);
    }
    rep(s,1<<n) {
        rep(i,n) rep(j,n) {
            if (((s>>i)&1) && ((s>>j)&1)) {
                dist_group[s] = max(dist_group[s],dist_two[i][j]);
            }
        }
    }
    rep(s,1<<n) rep(cnt,n) dp[s][cnt] = -1;
}

long rec(int s, int cnt) {
    /* cerr << "rec: " << s << " " << cnt << endl; */
    if (dp[s][cnt] != -1) {
        return dp[s][cnt];
    }
    const long inf = 1L<<60;
    if (cnt == 1) {
        dp[s][cnt] = dist_group[s];
        return dp[s][cnt];
    }
    dp[s][cnt] = inf;
    int t = s;
    do {
        dp[s][cnt] = min(dp[s][cnt],max(rec(t,cnt-1),dist_group[s-t]));
        t = (t-1)&s;
    } while (t != s);
    return dp[s][cnt];
}

int main() {
    cin >> n >> k;
    rep(i,n) cin >> x[i] >> y[i];

    init();
    long res = rec((1<<n)-1,k);
    cout << res << endl;
}
