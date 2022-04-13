#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

int n;
char c[100000];
vector<int> g[100000];

mint dp[3][100000];

void dfs(int v, int prev) {
    mint x = 1;
    mint y = 1;
    for (auto adj : g[v]) {
        if (adj == prev) continue;
        dfs(adj,v);
        if (c[v] == 'a') {
            x *= (dp[0][adj] + dp[2][adj]);
            y *= (dp[0][adj] + dp[1][adj] + 2*dp[2][adj]);
        }
        else if (c[v] == 'b') {
            x *= (dp[1][adj] + dp[2][adj]);
            y *= (dp[0][adj] + dp[1][adj] + 2*dp[2][adj]);
        }
        else throw;
    }
    if (c[v] == 'a') {
        dp[0][v] = x;
        dp[1][v] = 0;
        dp[2][v] = y - x;
    }
    else if (c[v] == 'b') {
        dp[0][v] = 0;
        dp[1][v] = x;
        dp[2][v] = y - x;
    }
    else throw;
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> c[i];
    for (int i = 0; i < n-1; i++) {
        int u,v;
        cin >> u >> v; u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }
    dfs(0,-1);
    cout << dp[2][0].val() << endl;
}
