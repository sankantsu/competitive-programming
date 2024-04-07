#include <iostream>
#include <vector>
#include <bitset>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    vector<vector<int>> a(n, vector<int>(m));
    rep(i,n) rep(j,m) cin >> a[i][j];

    constexpr int max_n = 2000;
    constexpr int max_v = 1000;
    vector<bitset<max_n>> bt(n);
    vector<bitset<max_n>> bs(max_v);

    rep(k,m) {
        rep(v,max_v) bs[v].reset();

        rep(i,n) {
            bs[a[i][k]].set(i);
        }
        rep(i,n) {
            bt[i] ^= bs[a[i][k]];
        }
    }

    long ans = 0;
    rep(i,n) for (int j = i+1; j < n; j++) {
        if (bt[i].test(j)) ans++;
    }
    cout << ans << endl;
}
