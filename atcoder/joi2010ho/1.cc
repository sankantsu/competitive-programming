// 1 - 旅人
// https://atcoder.jp/contests/joi2010ho/tasks/joi2010ho_a
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr long mod = 100000;

int n,m;
long d[200000];
int a[200000];

long sum[200000];

int main() {
    cin >> n >> m;
    rep(i,n-1) cin >> d[i];
    rep(i,m) cin >> a[i];

    rep(i,n+1) sum[i+1] = sum[i] + d[i];
    /* rep(i,n) cout << sum[i] << " "; */
    /* cout << endl; */

    int cur = 0;
    long res = 0;
    rep(i,m) {
        int next = cur + a[i];
        int l = min(cur,next);
        int r = max(cur,next);
        long dist = sum[r] - sum[l];
        /* cout << "cur,next,dist: " << cur << " " << next << " " << dist << endl; */
        res = (res+dist)%mod;
        cur = next;
    }
    cout << res << endl;
}
