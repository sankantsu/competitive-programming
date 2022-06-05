#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
vector<int> g[200000];

long query(int x, int k) {
    long res = 0;
    queue<pair<int,int>> q;
    set<int> visited;
    q.emplace(x,k);
    while(!q.empty()) {
        auto [y,d] = q.front(); q.pop();
        if (visited.find(y) != visited.end()) continue;
        res += y;
        visited.insert(y);
        if (d > 0) {
            for (auto adj : g[y]) {
                q.emplace(adj,d-1);
            }
        }
    }
    return res;
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int a,b;
        cin >> a >> b;
        g[a].push_back(b);
        g[b].push_back(a);
    }
    int q;
    cin >> q;
    rep(i,q) {
        int x,k;
        cin >> x >> k;
        cout << query(x,k) << endl;
    }
}
