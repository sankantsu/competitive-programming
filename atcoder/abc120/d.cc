// D - Decayed Bridges
// https://atcoder.jp/contests/abc120/tasks/abc120_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct uftree {
    uftree(int n) : par(n), group_size(n) {
        rep(i,n) {
            par[i] = i;
            group_size[i] = 1;
        }
    }
    int find(int x) {
        if (par[x] == x) return x;
        par[x] = find(par[x]);
        group_size[x] = group_size[par[x]];
        return par[x];
    }
    bool same(int x, int y) {
        return find(x) == find(y);
    }
    long unite(int x, int y) {
        x = find(x); y = find(y);
        if (x == y) return 0;
        long s1 = group_size[x];
        long s2 = group_size[y];
        par[x] = y;
        group_size[x] = s1+s2;
        group_size[y] = s1+s2;
        return s1*s2;
    }
    private:
    vector<int> par;
    vector<long> group_size;
};

const long max_n = 100000;

long n,m;
int a[max_n+10];
int b[max_n+10];

int main() {
    cin >> n >> m;
    rep(i,m) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }

    uftree uf(n);
    vector<long> res;
    long inc = n*(n-1)/2;
    for (int i = m-1; i >= 0; i--) {
        res.push_back(inc);
        long s = uf.unite(a[i],b[i]);
        inc -= s;
    }
    for_each(res.rbegin(),res.rend(),[](long x) {
            cout << x << endl;
        }
    );
}
