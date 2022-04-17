// E - 散歩
// https://atcoder.jp/contests/s8pc-1/tasks/s8pc_1_e
#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint1000000007;

int n,q;
int a[200000];
int c[200000];

mint dist[200000];

mint mod_pow(int a, int b) {
    if (b == 0) return 1;
    mint res = mod_pow(a,b>>1);
    return res*res*((b&1)?a:1);
}

int main() {
    cin >> n >> q;
    rep(i,n) cin >> a[i];
    rep(i,q) {
        cin >> c[i];
        c[i]--;
    }
    c[q] = 0;

    dist[0] = 0;
    rep(i,n-1) {
        mint d = mod_pow(a[i],a[i+1]);
        dist[i+1] = dist[i] + d;
    }
    mint res = 0;
    int cur = 0;
    rep(i,q+1) {
        int l = min(cur,c[i]);
        int r = max(cur,c[i]);
        res += dist[r] - dist[l];
        cur = c[i];
    }
    cout << res.val() << endl;
}
