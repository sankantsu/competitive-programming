// Building a Space Station
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1127
#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

template <typename Graph>
double prim(const Graph& g) {
    int n = g.size();
    const double inf = 1<<30;
    vector<bool> used(n,false);
    vector<double> mincost(n,inf);
    mincost[0] = 0;
    double cost = 0;
    while (true) {
        int v = -1;
        rep(i,n) {
            if (!used[i] && (v == -1 || mincost[i] < mincost[v])) {
                v = i;
            }
        }
        if (v == -1) break;
        used[v] = true;
        cost += mincost[v];
        rep(i,n) {
            mincost[i] = min(mincost[i],g[i][v]);
        }
    }
    return cost;
}

bool solve() {
    int n;
    cin >> n;
    if (n == 0) return false;
    vector<double> x(n);
    vector<double> y(n);
    vector<double> z(n);
    vector<double> r(n);

    auto dist = [&](int i, int j) {
        double dx = x[i]-x[j];
        double dy = y[i]-y[j];
        double dz = z[i]-z[j];
        double d = sqrt(dx*dx + dy*dy + dz*dz);
        double interval = max(d-r[i]-r[j],0.0);
        return interval;
    };

    rep(i,n) cin >> x[i] >> y[i] >> z[i] >> r[i];

    vector<vector<double>> d(n,vector<double>(n));
    rep(i,n) {
        rep(j,n) {
            d[i][j] = dist(i,j);
        }
    }
    cout << fixed << setprecision(3);
    cout << prim(d) << endl;
    return true;
}

int main() {
    while(solve()) {}
    return 0;
}
