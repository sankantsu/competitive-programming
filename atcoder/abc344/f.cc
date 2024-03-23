#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>
#include <queue>
#include <map>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// dp[i][j][p]: best state for (i,j) through point with score p
using State = pair<size_t, long>;  // move count, remaining money
State dp[80][80][80*80];

int main() {
    constexpr long inf = 1L<<60;

    long n;
    cin >> n;

    vector<vector<long>> p(n, vector<long>(n));
    rep(i,n) rep(j,n) cin >> p[i][j];

    vector<vector<long>> r(n, vector<long>(n, inf));
    rep(i,n) rep(j,n-1) cin >> r[i][j];

    vector<vector<long>> d(n, vector<long>(n, inf));
    rep(i,n-1) rep(j,n) cin >> d[i][j];

    // compress p
    set<long> pss;
    rep(i,n) rep(j,n) pss.insert(p[i][j]);
    const long m = pss.size();  // m <= n*n
    vector<long> ps;
    for (auto p : pss) ps.push_back(p);
    // rewrite with compresssed id
    rep(i,n) rep(j,n) p[i][j] = distance(pss.begin(), pss.lower_bound(p[i][j]));

    rep(i,n) rep(j,n) rep(k, m) dp[i][j][k] = make_pair(inf, inf);

    auto chmin = [&](auto& l, auto r) {
        l = min(l,r);
    };

    dp[0][0][p[0][0]] = make_pair(0, 0);
    rep(i,n) rep(j,n) rep(k,m) {
        if (dp[i][j][k].first == inf) continue;
        auto [cnt, t] = dp[i][j][k];
        t = -t;
        /* cerr << "i,j,k,cnt,t: " << i << " " << j << " " << k << " " << cnt << " " << t << endl; */
        auto move_to = [&,cnt=cnt,t=t](size_t ni, size_t nj, const auto& cost) {
            long pv = ps[k];
            size_t n_earn = (cost[i][j] - t + (pv - 1)) / pv;
            size_t nc = cnt + n_earn + 1;
            long nt = (t + n_earn*pv) - cost[i][j];
            /* cerr << "pv, n_earn: " << pv << " " << n_earn << endl; */
            chmin(dp[ni][nj][max(k, p[ni][nj])], make_pair(nc, -nt));
        };
        if (j < n - 1) move_to(i, j+1, r);
        if (i < n - 1) move_to(i+1, j, d);
    }

    size_t ans = inf;
    rep(k,m) {
        chmin(ans, dp[n-1][n-1][k].first);
    }
    cout << ans << endl;
}
