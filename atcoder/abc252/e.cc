#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
vector<tuple<int,long,int>> g[200010];

long dist[200010];

auto solve() {
    const long inf = 1L<<60;
    rep(i,n) dist[i] = inf;

    using P = tuple<long,int,int>;
    priority_queue<P,vector<P>,greater<P>> q;

    dist[0] = 0;
    q.emplace(0,0,-1);

    vector<int> road;
    while(!q.empty()) {
        auto [d,v,num] = q.top(); q.pop();
        if (d > dist[v]) continue;
        else {
            if (num != -1) road.push_back(num);
            for (auto [u,cost,nnum] : g[v]) {
                long nd = d + cost;
                if (nd < dist[u]) {
                    dist[u] = nd;
                    q.emplace(nd,u,nnum);
                }
            }
        }
    }
    return road;
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int a,b;
        long c;
        cin >> a >> b >> c;
        a--; b--;
        g[a].emplace_back(b,c,i+1);
        g[b].emplace_back(a,c,i+1);
    }

    auto v = solve();
    for (auto num : v) {
        cout << num << " ";
    }
    cout << endl;
}
