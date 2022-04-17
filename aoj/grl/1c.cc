#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;

using edge = pair<int,long>;
vector<edge> g[200];

constexpr long inf = 1L<<61;
long dp[200][200];

bool warshall_floyd() {
    rep(i,n) {
        rep(j,n) {
            dp[i][j] = inf;
        }
    }
    rep(i,n) {
        dp[i][i] = 0;
        for (auto [j,d] : g[i]) {
            dp[i][j] = d;
        }
    }
    rep(k,n) {
        rep(i,n) {
            rep(j,n) {
                dp[i][j] = min(dp[i][j],dp[i][k]+dp[k][j]);
            }
        }
    }
    rep(i,n) {
        if (dp[i][i] < 0) {
            return false;
        }
    }
    return true;
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int s,t;
        long d;
        cin >> s >> t >> d;
        g[s].emplace_back(t,d);
    }
    if (warshall_floyd()) {
        rep(i,n) {
            rep(j,n) {
                if (dp[i][j] > inf/2) {
                    cout << "INF";
                }
                else {
                    cout << dp[i][j];
                }
                cout << (j == n-1 ? "\n" : " ");
            }
        }
    }
    else {
        cout << "NEGATIVE CYCLE" << endl;
    }
}
