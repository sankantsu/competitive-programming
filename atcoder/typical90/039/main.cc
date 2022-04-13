// O(n) AC

#include <bits/stdc++.h>
#include <atcoder/all>

using namespace std;
using namespace atcoder;

int n;
vector<int> g[100000];

long dp[100000]; // 各頂点を根とする部分木のノード数

int dfs(int v, int prev) {
    int res = 1;
    for (auto u : g[v]) {
        if (u == prev) continue;
        res += dfs(u,v);
    }
    dp[v] = res;
    return res;
}

int main() {
    cin >> n;
    for (int i = 0; i < n-1; i++) {
        int a,b;
        cin >> a >> b; a--; b--;
        g[a].push_back(b);
        g[b].push_back(a);
    }
    dfs(0,-1);
    long res = 0;
    for (int i = 0; i < n; i++) {
        res += dp[i]*(n - dp[i]);
    }
    cout << res << endl;
}
