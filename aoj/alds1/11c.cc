#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

int n;
vector<int> g[101];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        int u,k,v;
        cin >> u >> k; u--;
        for (int j = 0; j < k; j++) {
            cin >> v; v--;
            g[u].push_back(v);
        }
    }
    const int inf = 100000;
    queue<int> q;
    vector<int> dist(n,inf);
    dist[0] = 0;
    q.push(0);
    while (!q.empty()) {
        int v = q.front(); q.pop();
        int d = dist[v];
        for (auto adj : g[v]) {
            if (dist[adj] == inf) {
                dist[adj] = d+1;
                q.push(adj);
            }
        }
    }
    for (int i = 0; i < n; i++) {
        if (dist[i] == inf) dist[i] = -1;
        cout << (i+1) << " " << dist[i] << endl;
    }
}
