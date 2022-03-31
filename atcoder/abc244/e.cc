#include <iostream>
#include <atcoder/modint>
#include <vector>

using namespace std;
using namespace atcoder;

using mint = modint998244353;

constexpr int max_k = 2000;
constexpr int max_n = 2000;

mint dp[max_k+1][max_n+1][2];

int main() {
    int n,m,k,s,t,x;
    cin >> n >> m >> k >> s >> t >> x;

    vector<vector<int>> g(n+1);
    for (int i = 0; i < m; i++) {
        int from, to;
        cin >> from >> to;
        g[from].push_back(to);
        g[to].push_back(from);
    }

    dp[0][s][0] = 1;
    for (int l = 0; l < k; l++) {
        for (int j = 1; j <= n; j++) {
            for (int r = 0; r <= 1; r++) {
                for (auto adj : g[j]) {
                    if (j == x) {
                        dp[l+1][x][r] += dp[l][adj][1-r];
                    }
                    else {
                        dp[l+1][j][r] += dp[l][adj][r];
                    }
                }
            }
        }
    }
    
    cout << dp[k][t][0].val() << endl;
}
