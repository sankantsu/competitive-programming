#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long inf = 1L<<61;

int v,e,r;

using edge = pair<int,long>;
vector<edge> g[100000];

long dist[100000];

using st = pair<long,int>;
priority_queue<st,vector<st>,greater<st>> q;

void dijkstra() {
    dist[r] = 0L;
    q.emplace(0L,r);
    while (!q.empty()) {
        auto [d,s] = q.top(); q.pop();
        if (d > dist[s]) continue;
        for (auto [adj,cost] : g[s]) {
            long nd = d+cost;
            if (nd < dist[adj]) {
                dist[adj] = nd;
                q.emplace(nd,adj);
            }
        }
    }
}

int main() {
    cin >> v >> e >> r;
    rep(i,e) {
        int s,t,d;
        cin >> s >> t >> d;
        g[s].emplace_back(t,d);
    }
    rep(i,v) {
        dist[i] = inf;
    }
    dijkstra();
    rep(i,v) {
        if (dist[i] == inf) {
            cout << "INF" << endl;
        }
        else {
            cout << dist[i] << endl;
        }
    }
}
