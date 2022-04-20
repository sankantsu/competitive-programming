// finals - 本選会場
// https://atcoder.jp/contests/joisc2010/tasks/joisc2010_finals
// https://www.ioi-jp.org/camp/2010/2010-sp-tasks/2010-sp-day3_22.pdf
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct uftree {
    uftree(int n) : par(n) {
        rep(i,n) par[i] = i;
    }
    int find(int x) {
        if (par[x] == x) return x;
        return par[x] = find(par[x]);
    }
    int same(int x, int y) {
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

template <typename Edges>
auto kruskal(int n, const Edges& es) {
    uftree uf(n);
    Edges res;
    for (auto e : es) {
        auto [c,a,b] = e;
        if (!uf.same(a,b)) {
            res.push_back(e);
            uf.unite(a,b);
        }
    }
    return res;
}

int main() {
    int n,m,k;
    cin >> n >> m >> k;
    using edge = tuple<int,int,int>;
    vector<edge> es;
    rep(i,m) {
        int a,b,c;
        cin >> a >> b >> c; a--; b--;
        es.emplace_back(c,a,b);
    }
    sort(es.begin(),es.end());
    auto mintree = kruskal(n,es);
    rep(i,k-1) {
        mintree.pop_back();
    }
    long res = 0;
    for(auto [c,a,b] : mintree) res += c;
    cout << res << endl;
}
