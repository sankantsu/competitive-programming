#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <atcoder/modint.hpp>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

mint dp[2][2001][2001];  // dp[X の到達回数が奇数?][移動回数][現在位置] = 何通り?

int main() {
    long n, m, k, s, t, x;
    cin >> n >> m >> k >> s >> t >> x;
    s--; t--; x--;

    vector<vector<long>> g(n);
    rep(i,m) {
        long u, v;
        cin >> u >> v;
        u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }

    dp[0][0][s] = 1;

    rep(i, k) rep(u, n) rep(t, 2) {
        for (auto v : g[u]) {
            long nt = (v == x) ? 1 - t : t;
            long ni = i + 1;
            dp[nt][ni][v] += dp[t][i][u];
            /* cerr << "nt,ni,u,v,value: " << nt << " " << ni << " " << u << " " << v << " " << dp[t][i][u].val() << endl; */
        }
    }
    /* rep(i, k+1) rep(u, n) rep(t, 2) { */
    /*     cerr << "i,u,t,dp[t][i][u]: " << i << " " << u << " " << t << " " << dp[t][i][u].val() << endl; */
    /* } */
    cout << dp[0][k][t].val() << endl;
}
