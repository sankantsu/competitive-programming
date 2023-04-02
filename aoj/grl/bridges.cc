#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
vector<int> g[100000];

bool visit[100000];

int par[100000];
int ord[100000];
int low[100000];

void dfs(int v, int p, int k) {
    visit[v] = true;
    par[v] = p;
    ord[v] = k;
    low[v] = k;
    for (auto u : g[v]) {
        if (u == p) continue;
        if (visit[u]) {
            low[v] = min(low[v],ord[u]);
        }
        else {
            dfs(u,v,k+1);
            low[v] = min(low[v],low[u]);
        }
    }
}

auto find_bridges() {
    dfs(0,-1,0); // init
    vector<pair<int,int>> br;
    for (int i = 0; i < n; i++) {
        if (par[i] == -1) continue;
        if (ord[i] == low[i]) {
            int v = min(i,par[i]);
            int u = max(i,par[i]);
            br.emplace_back(v,u);
        }
    }
    sort(br.begin(),br.end());
    return br;
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int s,t;
        cin >> s >> t;
        g[s].push_back(t);
        g[t].push_back(s);
    }

    auto br = find_bridges();
    for (auto [v,u] : br) {
        cout << v << " " << u << endl;
    }
}
