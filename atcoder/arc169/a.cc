#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n), p(n);
    rep(i,n) cin >> a[i];
    rep(i,n-1) {
        cin >> p[i+1];
        p[i+1]--;
    }

    vector<vector<long>> g(n);
    rep(i,n-1) {
        g[p[i+1]].push_back(i+1);
    }

    vector<long> distance(n, -1);
    // bfs
    using P = pair<size_t, long>;  // node, distance
    queue<P> q;
    q.emplace(0, 0);
    while (!q.empty()) {
        auto [u, d] = q.front();
        cerr << "u,d: " << u << " " << d << endl;
        q.pop();
        if (distance[u] < d) distance[u] = d;
        for (auto v : g[u]) {
            q.emplace(v, d + 1);
        }
    }

    vector<long> v(n);
    rep(i, n) {
        if (distance[i] < 0) continue;
        v[distance[i]] += a[i];
    }

    long ans = 0;
    rep(i,n) {
        if (v[n - 1 - i] != 0) {
            ans = v[n - 1 - i];
            break;
        }
    }
    if (ans == 0) {
        cout << "0" << endl;
    }
    else if (ans > 0) {
        cout << "+" << endl;
    }
    else {
        cout << "-" << endl;
    }
}
