#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
vector<int> g[200];

int t = 1;
int d[200];
int f[200];

void dfs(int v, int prev) {
    d[v] = t;
    t++;
    for (auto adj : g[v]) {
        if (adj == prev) continue;
        if (d[adj] == 0) {
            dfs(adj,v);
        }
    }
    f[v] = t;
    t++;
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        int u,k;
        cin >> u >> k;
        u--;
        for (int j = 0; j < k; j++) {
            int v;
            cin >> v;
            v--;
            g[u].push_back(v);
        }
    }
    for (int i = 0; i < n; i++) {
        if (d[i] == 0) {
            dfs(i,-1);
        }
    }
    for (int i = 0; i < n; i++) {
        cout << (i+1) << " " << d[i] << " " << f[i] << endl;
    }
}
