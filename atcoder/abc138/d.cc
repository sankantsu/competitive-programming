// D - Ki
// https://atcoder.jp/contests/abc138/tasks/abc138_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,q;
vector<int> g[300000];

long add[300000];
long val[300000];

void dfs(int v, int par, int x) {
    x += add[v];
    val[v] = x;
    for (auto adj : g[v]) {
        if (adj == par) continue;
        dfs(adj,v,x);
    }
}

int main() {
    cin >> n >> q;
    rep(i,n-1) {
        int s,t;
        cin >> s >> t; s--; t--;
        g[s].push_back(t);
        g[t].push_back(s);
    }
    rep(i,q) {
        int p;
        long x;
        cin >> p >> x; p--;
        add[p] += x;
    }
    dfs(0,-1,0);
    rep(i,n) {
        cout << val[i] << endl;
    }
}
