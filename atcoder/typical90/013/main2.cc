#include <iostream>
#include <vector>
#include <queue>

using namespace std;

constexpr long inf = 1L<<60;

template <typename Graph>
auto dijkstra(const Graph &g, long s) {
    vector<long> dist(g.size(),inf);
    queue<pair<long,long>> q;
    dist[s] = 0;
    q.emplace(s,0);
    while (!q.empty()) {
        auto p = q.front(); q.pop();
        long v = p.first;
        long d = p.second;
        if (dist[v] < d) {
            continue;
        }
        dist[v] = d;
        for (auto [adj,c] : g[v]) {
            if (c+dist[v] < dist[adj]) {
                q.emplace(adj,c+dist[v]);
            }
        }
    }
    return dist;
}

int main() {
    long n,m;
    cin >> n >> m;

    vector<vector<pair<long,long>>> g(n);
    for (long i = 0; i < m; i++) {
        long a,b,c;
        cin >> a >> b >> c; a--; b--;
        g[a].emplace_back(b,c);
        g[b].emplace_back(a,c);
    }

    auto d1 = dijkstra(g,0);
    auto d2 = dijkstra(g,n-1);
    for (int k = 0; k < n; k++) {
        cout << (d1[k] + d2[k]) << endl;
    }
}
