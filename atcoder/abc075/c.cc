#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
int a[100];
int b[100];

struct uftree {
    uftree(int n) : par(n) {
        rep(i,n) {
            par[i] = i;
        }
    }
    int find(int x) {
        if (par[x] == x) return x;
        par[x] = find(par[x]);
        return par[x];
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
    cin >> n >> m;
    rep(i,m) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }
    int res = 0;
    rep(i,m) {
        uftree uf(n);
        rep(j,m) {
            if (i == j) {
                continue;
            }
            uf.unite(a[j],b[j]);
        }
        if (!uf.same(a[i],b[i])) {
            res++;
        }
    }
    cout << res << endl;
}
