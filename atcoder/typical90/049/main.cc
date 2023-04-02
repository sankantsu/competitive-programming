#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
long c[200000];
int l[200000];
int r[200000];

struct edge {
    int from;
    int to;
    long cost;
};

struct uftree {
    uftree(int n) : par(n) {
        rep(i,n) par[i] = i;
    }
    int find(int x) {
        if (par[x] == x) return x;
        return par[x] = find(par[x]);
    }
    bool same(int x, int y) {
        return find(x) == find(y);
    }
    void unite(int x, int y) {
        x = find(x); y = find(y);
        if (x == y) return;
        par[x] = y;
    }
    private:
    vector<int> par;
};

int main() {
    cin >> n >> m;
    rep(i,m) {
        cin >> c[i] >> l[i] >> r[i];
        l[i]--; r[i]--;
    }

    vector<edge> ve;
    rep(i,m) {
        ve.push_back(edge{l[i],r[i]+1,c[i]});
    }
    sort(ve.begin(),ve.end(),[](edge lhs, edge rhs) { return lhs.cost < rhs.cost; });

    uftree uf(n+1);
    long res = 0;
    for (auto e : ve) {
        if (!uf.same(e.from,e.to)) {
            uf.unite(e.from,e.to);
            res += e.cost;
        }
    }
    
    bool check = true;
    rep(i,n) {
        if (!uf.same(0,i)) {
            check = false;
            break;
        }
    }
    if (check) {
        cout << res << endl;
    }
    else {
        cout << -1 << endl;
    }
}
