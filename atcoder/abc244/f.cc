#include <iostream>
#include <vector>
#include <queue>
#include <utility>
#include <cmath>

using namespace std;

constexpr int max_n = 17;
constexpr int max_N = 0x1<<17;
constexpr int inf = pow(10,9);

int dist[max_N][max_n];

int main() {
    int n, m;
    cin >> n >> m;

    vector<int> graph[max_n];
    for (int i = 0; i < m; i++) {
        int u,v;
        cin >> u >> v;
        u--; v--;
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    queue<pair<int,int>> q;

    for (int i = 0; i < 1<<n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == 1<<j) {
                dist[i][j] = 1;
                q.emplace(i,j);
            }
            else {
                dist[i][j] = inf;
            }
        }
    }

    // bfs
    while (!q.empty()) {
        auto pair = q.front(); q.pop();
        int s = pair.first;
        int v = pair.second;
        for (auto u : graph[v]) {
            int ns = s ^ (1<<u);
            if (dist[ns][u] < inf) {
                continue;
            }
            dist[ns][u] = dist[s][v] + 1;
            q.emplace(ns,u);
        }
    }

    int ans = 0;
    for (int i = 1; i < 1<<n; i++) {
        int min_l = inf;
        for (int j = 0; j < n; j++) {
            if (dist[i][j] < min_l) {
                min_l = dist[i][j];
            }
        }
        ans += min_l;
    }
    cout << ans << endl;
}
