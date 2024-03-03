#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    vector<long> u(m), v(m);
    rep(i,m) {
        cin >> u[i] >> v[i];
        u[i]--; v[i]--;
    }

    vector<long> w(n);
    rep(i,n) {
        cin >> w[i];
    }

    vector<vector<long>> g(n);
    rep(i,m) {
        if (w[u[i]] == w[v[i]]) continue;
        if (w[u[i]] > w[v[i]]) {
            g[u[i]].push_back(v[i]);
        }
        else {
            g[v[i]].push_back(u[i]);
        }
    }

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    using P = pair<long,long>;  // cost, node
    priority_queue<P> queue;
    rep(i,n) {
        queue.emplace(-w[i], i);
    }

    vector<long> dp(n);
    while(!queue.empty()) {
        auto [_, v] = queue.top();
        queue.pop();

        constexpr long max_w = 5000;
        vector<long> dp2(max_w+1);
        rep(i, g[v].size()) {
            long u = g[v][i];
            for (long j = max_w; j >= 1; j--) {
                if (j >= w[u]) {
                    dp2[j] = max(dp2[j], dp[u] + dp2[j - w[u]]);
                }
            }
        }
        dp[v] = 1 + dp2[w[v] - 1];
    }

    long ans = 0;
    rep(i,n) {
        ans += a[i]*dp[i];
    }
    cout << ans << endl;
}
