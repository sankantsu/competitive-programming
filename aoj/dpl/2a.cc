#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

using edge = pair<int,int>;
using state = pair<int,int>;
using ds = pair<int,state>;

int v,e;
vector<edge> g[20];

const int inf = 1<<30;
int dp[1<<20][20];

void travel() {
    for (int k = 0; k < (1<<v); k++) {
        for (int i = 0; i < v; i++) {
            dp[k][i] = inf;
        }
    }
    priority_queue<ds,vector<ds>,greater<ds>> q;
    dp[0][0] = 0;
    q.emplace(0,make_pair(0,0));
    while (!q.empty()) {
        auto [d,st] = q.top(); q.pop();
        auto [k,v] = st;
        /* cout << "rec " << d << " " << hex << k << " " << dec << v << endl; */
        if (v == 0 && k != (1<<v)-1) continue;
        if (d > dp[k][v]) continue;
        for (auto [adj,cost] : g[v]) {
            /* cout << "adj,cost: " << adj << " " << cost << endl; */
            if ((k>>adj)&1) continue;
            int nk = k|(1<<adj);
            int nd = d+cost;
            /* cout << "nk,nd: " << nk << " " << nd << endl; */
            /* cout << dp[nk][adj] << endl; */
            if (nd < dp[nk][adj]) {
                dp[nk][adj] = nd;
                q.emplace(nd,make_pair(nk,adj));
            }
        }
    }
}

int main() {
    cin >> v >> e;
    for (int i = 0; i < e; i++) {
        int s,t,d;
        cin >> s >> t >> d;
        g[s].emplace_back(t,d);
        /* g[t].emplace_back(s,d); */
    }
    travel();
    if (dp[(1<<v)-1][0] == inf) {
        cout << -1 << endl;
    }
    else {
        cout << dp[(1<<v)-1][0] << endl;
    }
}
