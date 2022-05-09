// E - タクシー
// https://atcoder.jp/contests/joi2014yo/tasks/joi2014yo_e
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 5000;
long n,k;
long c[max_n+10];
long r[max_n+10];
vector<long> g[max_n+10];

long dist[max_n+10];

void dijkstra() {
    const long inf = 1L<<60;
    rep(i,n) dist[i] = inf;

    using state = pair<long,long>;
    priority_queue<state,vector<state>,greater<state>> q; // dist, v
    q.emplace(0,0);
    dist[0] = 0;
    while(!q.empty()) {
        auto [d,v] = q.top(); q.pop();
        /* cout << "d,v: " << d << " " << v << endl; */
        long nd = d + c[v];
        if (d > dist[v]) continue;
        // bfs
        vector<bool> used(n,false);
        queue<state> q2; // remaining move, u
        q2.emplace(r[v],v);
        while(!q2.empty()) {
            auto [cnt,u] = q2.front(); q2.pop();
            /* cout << "cnt,u: " << cnt << " " << u << endl; */
            if (used[u]) continue;
            used[u] = true;
            if (nd < dist[u]) {
                dist[u] = nd;
                q.emplace(nd,u);
            }
            if (cnt > 0) {
                for (auto adj : g[u]) {
                    q2.emplace(cnt-1,adj);
                }
            }
        }
    }
}

int main() {
    cin >> n >> k;
    rep(i,n) cin >> c[i] >> r[i];
    rep(i,k) {
        long u,v;
        cin >> u >> v; u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }
    dijkstra();
    cout << dist[n-1] << endl;
}
