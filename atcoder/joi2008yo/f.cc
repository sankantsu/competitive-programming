// F - 船旅
// https://atcoder.jp/contests/joi2008yo/tasks/joi2008yo_f
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
using edge = pair<long,int>; // cost,to
vector<edge> g[200];

long dijkstra(int a, int b) {
    constexpr long inf = 1L<<60;
    vector<long> dist(n,inf);
    using state = pair<long,int>; // cost,vertex
    priority_queue<state,vector<state>,greater<state>> q;
    dist[a] = 0;
    q.emplace(0,a);
    while(!q.empty()) {
        auto [d,v] = q.top(); q.pop();
        if (v == b) {
            break;
        }
        if (d > dist[v]) continue;
        for (auto [cost,adj] : g[v]) {
            long nd = d+cost;
            if (nd < dist[adj]) {
                dist[adj] = nd;
                q.emplace(nd,adj);
            }
        }
    }
    if (dist[b] == inf) {
        return -1;
    }
    else {
        return dist[b];
    }
}

int main() {
    cin >> n >> k;
    rep(i,k) {
        int t;
        cin >> t;
        if (t == 0) {
            int a,b;
            cin >> a >> b; a--; b--;
            long cost = dijkstra(a,b);
            cout << cost << endl;
        }
        else {
            int c,d;
            long e;
            cin >> c >> d >> e; c--; d--;
            g[c].emplace_back(e,d);
            g[d].emplace_back(e,c);
        }
    }
}
