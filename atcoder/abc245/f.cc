#include <iostream>
#include <vector>
#include <set>

using namespace std;

constexpr int max_n = 200000;

int n,m;
vector<int> g[max_n];

bool to_loop[max_n];
bool reached[max_n];

void dfs(int v, set<int> s) {
    // debug
    /* cout << "dfs" << endl; */
    /* cout << "v: " << v << endl; */
    /* cout << "s: "; */
    /* for (auto u : s) { */
    /*     cout << u << " "; */
    /* } */
    /* cout << endl; */

    if (reached[v]) {
        if (s.find(v) != s.end()) {
            // cout << "found loop" << endl;
            for (auto u : s) {
                to_loop[u] = true;
            }
        }
        return;
    }
    else {
        reached[v] = true;
        if (g[v].empty()) {
            return;
        }
        s.insert(v);
        for (auto adj : g[v]) {
            dfs(adj,s);
        }
    }
}

int main() {
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        int u,v;
        cin >> u >> v;
        u--; v--;
        g[u].push_back(v);
    }

    for (int v = 0; v < n; v++) {
        if (!reached[v]) {
            dfs(v,set<int>{});
        }
    }

    int ans = 0;
    for (int v = 0; v < n; v++) {
        if (to_loop[v]) {
            ans++;
        }
    }

    cout << ans << endl;
}
