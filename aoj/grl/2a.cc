// minimum spanning tree (kruskal)
#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;

using edge = tuple<long,int,int>; // cost, from, to
vector<edge> edges;

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

auto kruskal() {
    vector<edge> mstree;
    uftree uf(n);
    sort(edges.begin(),edges.end());
    for (auto e : edges) {
        auto [cost,from,to] = e;
        if (!uf.same(from,to)) {
            mstree.push_back(e);
            uf.unite(from,to);
        }
    }
    return mstree;
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        int s,t;
        long w;
        cin >> s >> t >> w;
        edges.emplace_back(w,s,t);
    }
    auto ms = kruskal();
    long sum = 0;
    for (auto [cost,from,to] : ms) {
        sum += cost;
    }
    cout << sum << endl;
}
