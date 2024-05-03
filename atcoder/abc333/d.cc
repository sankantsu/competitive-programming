#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<vector<int>> g(n);
    rep(i,n-1) {
        int u, v;
        cin >> u >> v;
        u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }

    vector<int> size(n);
    auto dfs = [&](auto self, int u, int p) -> int {
        int s = 1;
        for (auto v : g[u]) {
            if (v == p) continue;
            s += self(self, v, u);
        }
        size[u] = s;
        return s;
    };
    dfs(dfs, 0, -1);

    int ans = n;
    if (g[0].size() == 1) ans = 1;
    else {
        for (auto v : g[0]) ans = min(ans, n - size[v]);
    }
    cout << ans << endl;
}
