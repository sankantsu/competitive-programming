#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,q;

struct uftree {
    public:
    uftree(int n) : par(n) {
        rep(i,n) {
            par[i] = i;
        }
    }
    int find(int x) {
        if (par[x] == x) return x;
        else {
            par[x] = find(par[x]);
            return par[x];
        }
    }
    bool same(int x, int y) {
        return find(x) == find(y);
    }
    void unite(int x, int y) {
        x = find(x);
        y = find(y);
        if (x == y) return;
        par[x] = y;
    }
    private:
    vector<int> par;
};

int main() {
    cin >> n >> q;
    uftree uf(n);
    rep(i,q) {
        int com,x,y;
        cin >> com >> x >> y;
        if (com == 0) {
            uf.unite(x,y);
        }
        else {
            cout << static_cast<int>(uf.same(x,y)) << endl;
        }
    }
}
