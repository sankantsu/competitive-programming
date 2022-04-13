#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
using edge = pair<int,int>;
vector<edge> g[300];

constexpr int inf = 1<<28;
int dist[300][300];

void warshall_floyd() {
    rep(i,n) {
        rep(j,n) {
            dist[i][j] = inf;
        }
    }
    rep (i,n) {
        dist[i][i] = 0;
        for (auto [j,t] : g[i]) {
            dist[i][j] = t;
        }
    }
    rep(k,n) {
        rep(i,n) {
            rep (j,n) {
                dist[i][j] = min(dist[i][j],dist[i][k]+dist[k][j]);
            }
        }
    }
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int a,b,t;
        cin >> a >> b >> t; a--; b--;
        g[a].emplace_back(b,t);
        g[b].emplace_back(a,t);
    }
    warshall_floyd();
    int res = inf;
    rep(i,n) {
        int mx = -1;
        rep(j,n) {
            mx = max(mx,dist[i][j]);
        }
        res = min(res,mx);
    }
    cout << res << endl;
}
