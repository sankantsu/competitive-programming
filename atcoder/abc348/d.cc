#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <queue>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long h, w;
    cin >> h >> w;

    vector<string> a(h);
    rep(i,h) cin >> a[i];

    long n;
    cin >> n;
    vector<long> r(n), c(n), e(n);
    rep(i,n) {
        cin >> r[i] >> c[i] >> e[i];
        r[i]--; c[i]--;
    }

    long si, sj;
    long ti, tj;
    rep(i,h) rep(j,w) {
        if (a[i][j] == 'S') {
            si = i;
            sj = j;
        }
        else if (a[i][j] == 'T') {
            ti = i;
            tj = j;
        }
    }

    map<pair<size_t, size_t>, size_t> mp;  // row, col -> id
    rep(i,n) {
        auto p = make_pair(r[i], c[i]);
        mp[p] = i;
    }

    // if there is no medicine at start point, he cannot move
    long s;
    {
        auto p = make_pair(si, sj);
        if (mp.find(p) == mp.end()) {
            cout << "No" << endl;
            return 0;
        }
        else {
            s = mp[p];
        }
    }

    // add goal node
    {
        auto p = make_pair(ti, tj);
        mp[p] = n;
    }

    // add edges
    vector<vector<size_t>> g(n+1);
    vector<vector<bool>> visited(h, vector<bool>(w));
    long dx[4] = {0, 1, 0, -1};
    long dy[4] = {1, 0, -1, 0};
    rep(k,n) {
        // bfs
        rep(i,h) rep(j,w) visited[i][j] = false;
        using S = tuple<long, long, long>;  // row, col, remaining energy
        queue<S> q;
        visited[r[k]][c[k]] = true;
        q.emplace(r[k], c[k], e[k]);
        while (!q.empty()) {
            auto [x, y, e] = q.front();
            q.pop();
            auto p = make_pair(x, y);
            if (mp.find(p) != mp.end()) {
                size_t l = mp[p];
                if (k != l) g[k].push_back(l);
            }
            if (e > 0) {
                rep(dir, 4) {
                    long nx = x + dx[dir];
                    long ny = y + dy[dir];
                    long ne = e - 1;
                    if (nx < 0 || h <= nx || ny < 0 || w <= ny || a[nx][ny] == '#') continue;
                    if (visited[nx][ny]) continue;
                    visited[nx][ny] = true;
                    q.emplace(nx, ny, ne);
                }
            }
        }
    }

    /* rep(u,n) { */
    /*     for (auto v : g[u]) { */
    /*         cerr << u << " -> " << v << endl; */
    /*     } */
    /* } */

    // bfs (node)
    bool ans = false;
    vector<bool> vs(n+1);
    queue<long> q;
    vs[s] = true;
    q.push(s);
    while(!q.empty()) {
        long u = q.front();
        if (u == n) {  // goal
            ans = true;
            break;
        }
        q.pop();
        for (auto v : g[u]) {
            if (vs[v]) continue;
            vs[v] = true;
            q.push(v);
        }
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
