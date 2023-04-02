#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
int K[100000];
vector<int> R[100000];

vector<int> g[200000];
long dist[200000];

void bfs() {
    const long inf = 1L<<60;
    rep(i,n+m) dist[i] = inf;
    using P = pair<int,long>;
    queue<P> q;
    dist[0] = 0;
    q.emplace(0,0);
    while(!q.empty()) {
        auto [v,d] = q.front(); q.pop();
        for (auto u : g[v]) {
            if (d+1 < dist[u]) {
                dist[u] = d+1;
                q.emplace(u,d+1);
            }
        }
    }
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        cin >> K[i];
        rep(j,K[i]) {
            int r;
            cin >> r; r--;
            R[i].push_back(r);
        }
    }

    rep(i,m) {
        for (auto r : R[i]) {
            g[r].push_back(n+i);
            g[n+i].push_back(r);
        }
    }

    bfs();

    rep(i,n) {
        long takahashi = (dist[i] > 2*n) ? -1 : dist[i]/2;
        cout << takahashi << endl;
    }
}
