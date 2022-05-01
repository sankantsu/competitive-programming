#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 100000;

int n,m,k,s;
long p,q;
int c[max_n];

vector<int> g[max_n];

bool invaded[max_n];
bool dangerous[max_n];
long dist[max_n];

void find_dangerous_cities() {
    rep(i,n) dangerous[i] = false;
    queue<pair<int,int>> queue;
    rep(i,k) {
        queue.emplace(c[i],s);
    }
    while(!queue.empty()) {
        auto [v,d] = queue.front(); queue.pop();
        if (dangerous[v]) continue;
        dangerous[v] = true;
        if (d == 0) continue;
        for (auto u : g[v]) {
            queue.emplace(u,d-1);
        }
    }
}

void dijkstra() {
    const long inf = 1L<<60;
    rep(i,n) dist[i] = inf;
    using st = pair<long,int>;
    priority_queue<st,vector<st>,greater<st>> queue;
    dist[0] = 0;
    queue.emplace(0,0);
    while(!queue.empty()) {
        auto [d,v] = queue.top(); queue.pop();
        if (d > dist[v]) continue;
        for (auto u : g[v]) {
            if (invaded[u]) continue;
            long cost = (dangerous[u]) ? q : p;
            if (u == n-1) cost = 0;
            if (d+cost < dist[u]) {
                dist[u] = d+cost;
                queue.emplace(d+cost,u);
            }
        }
    }
}

int main() {
    cin >> n >> m >> k >> s;
    cin >> p >> q;
    rep(i,k) {
        cin >> c[i];
        c[i]--;
    }
    rep(i,m) {
        int from,to;
        cin >> from >> to; from--; to--;
        g[from].push_back(to);
        g[to].push_back(from);
    }
    rep(i,n) invaded[i] = false;
    rep(i,k) invaded[c[i]] = true;
    find_dangerous_cities();
    dijkstra();
    cout << dist[n-1] << endl;
}
