#include <iostream>
#include <set>
#include <queue>
#include <vector>

using namespace std;

constexpr int max_n = 200000;

int n,m;
set<int> g[max_n];
set<int> g_inv[max_n];

int main() {
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        int u,v;
        cin >> u >> v; u--; v--;
        g[u].insert(v);
        g_inv[v].insert(u);
    }

    queue<int> q;
    for (int i = 0; i < n; i++) {
        q.push(i);
    }

    vector<bool> removed(n);
    while (!q.empty()) {
        int u = q.front(); q.pop();
        if (removed[u]) {
            continue;
        }
        if (g[u].size() == 0) {
            removed[u] = true;
            for (auto adj : g_inv[u]) {
                g[adj].erase(u);
                q.push(adj);
            }
        }
    }

    int cnt = 0;
    for (int i = 0; i < n; i++) {
        if (!removed[i]) cnt++;
    }

    cout << cnt << endl;
}
