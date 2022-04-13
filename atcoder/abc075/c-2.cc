// lowlink
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
vector<int> g[100];

bool visit[100];
int ord[100];
int low[100];

using edge = pair<int,int>;
vector<edge> bridge;

void dfs(int v, int prev, int k) {
    visit[v] = true;
    ord[v] = k;
    low[v] = k;
    for (auto to : g[v]) {
        if (!visit[to]) {
            dfs(to,v,k+1);
            low[v] = min(low[v],low[to]);
            if (ord[v] < low[to]) {
                bridge.emplace_back(v,to);
            }
        }
        else if (to != prev) {
            low[v] = min(low[v],ord[to]);
        }
    }
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int u,v;
        cin >> u >> v; u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }
    dfs(0,-1,0);
    /* rep(i,n) { */
    /*     cout << "i,ord,low: " << i << " " << ord[i] << " " << low[i] << endl; */
    /* } */
    cout << bridge.size() << endl;
}
