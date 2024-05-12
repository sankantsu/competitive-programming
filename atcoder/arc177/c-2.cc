#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <queue>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

using P = pair<int, int>;  // (i, j) on grid
int dx[4] = {1, 0, -1, 0};
int dy[4] = {0, 1, 0, -1};

auto bfs(int n, char c, vector<string>& s) {
    /* cerr << "bfs for " << c << endl; */
    using S = tuple<int, int, int>;  // -cost, x, y
    const int inf = 1<<29;
    priority_queue<S> q;
    vector<vector<int>> dist(n, vector<int>(n, inf));
    int sx = 0;
    int sy = (c == 'R') ? 0 : n - 1;
    int gx = n - 1;
    int gy = (c == 'R') ? n - 1 : 0;
    q.emplace(0, sx, sy);
    dist[sx][sy] = 0;
    while (!q.empty()) {
        auto [_, x,y] = q.top(); q.pop();
        /* cerr << "x,y,dist: " << x << " " << y << " " << dist[x][y] << endl; */
        if (x == gx && y == gy) {
            break;
        }
        rep(d, 4) {
            int nx = x + dx[d];
            int ny = y + dy[d];
            if (nx < 0 || n <= nx || ny < 0 || n <= ny) continue;
            if (dist[nx][ny] < inf) continue;
            int nd;
            if (s[nx][ny] == c) {
                nd = dist[x][y];
            } else {
                nd = dist[x][y] + 1;
            }
            dist[nx][ny] = nd;
            q.emplace(-nd, nx, ny);
        }
    }
    return dist[gx][gy];
}

int solve(int n, char c, vector<string>& s) {
    int d = bfs(n, c, s);
    /* cerr << "d: " << d << endl; */
    return d;
}

int main() {
    int n;
    cin >> n;

    vector<string> s(n);
    rep(i,n) cin >> s[i];

    int ans = 0;
    ans += solve(n, 'R', s);
    ans += solve(n, 'B', s);
    cout << ans << endl;
}
