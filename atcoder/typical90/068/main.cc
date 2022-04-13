#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n, q;
int t[100000];
int x[100000];
int y[100000];
long v[100000];

struct uftree {
    uftree(int n) : par(n) {
        init();
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
    void init() {
        for (int i = 0; i < n; i++) {
            par[i] = i;
        }
    }
    vector<int> par;
};

long sum[100000]; // sum[i] = a[i] + a[i+1]
long potential[100000]; // a[i] - a[0] where all nodes connected

int main() {
    cin >> n >> q;
    for (int i = 0; i < q; i++) {
        cin >> t[i] >> x[i] >> y[i] >> v[i];
        x[i]--; y[i]--;
    }
    for (int i = 0; i < q; i++) {
        if (t[i] == 0)
            sum[x[i]] = v[i];
    }
    for (int i = 0; i < n-1; i++) {
        potential[i+1] = sum[i] - potential[i];
    }
    /* cout << "sum: "; */
    /* for (int i = 0; i < n; i++) { */
    /*     cout << sum[i] << " "; */
    /* } cout << endl; */
    /* cout << "potential: "; */
    /* for (int i = 0; i < n; i++) { */
    /*     cout << potential[i] << " "; */
    /* } cout << endl; */
    uftree u(n);
    for (int i = 0; i < q; i++) {
        if (t[i] == 0) {
            u.unite(x[i],y[i]);
        }
        else if (t[i] == 1) {
            if (!u.same(x[i],y[i])) {
                cout << "Ambiguous" << endl;
            }
            else if ((x[i]+y[i])%2 == 0) {
                long val = potential[y[i]] + (v[i] - potential[x[i]]);
                cout << val << endl;
            }
            else {
                long val = potential[y[i]] - (v[i] - potential[x[i]]);
                cout << val << endl;
            }
        }
        else {
            throw;
        }
    }
}
