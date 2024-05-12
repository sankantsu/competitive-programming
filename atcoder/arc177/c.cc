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
    queue<P> q;
    int start = -1;
    int goal = -1;
    vector<vector<P>> res;
    vector<vector<bool>> used(n, vector<bool>(n));
    rep(i,n) rep(j,n) {
        if (used[i][j] || s[i][j] != c) {
            continue;
        }
        vector<P> side;
        q.emplace(i, j);
        used[i][j] = true;
        while (!q.empty()) {
            auto [x,y] = q.front(); q.pop();
            if (c == 'R') {
                if (x == 0 && y == 0) {
                    start = res.size();
                } else if (x == n-1 && y == n-1) {
                    goal = res.size();
                }
            } else {
                if (x == 0 && y == n-1) {
                    start = res.size();
                } else if (x == n-1 && y == 0) {
                    goal = res.size();
                }
            }
            bool is_side = false;
            rep(d, 4) {
                int nx = x + dx[d];
                int ny = y + dy[d];
                if (nx < 0 || n <= nx || ny < 0 || n <= ny) continue;
                if (used[nx][ny]) continue;
                if (!is_side && s[nx][ny] != c) {
                    side.emplace_back(x, y);
                    is_side = true;
                } else if (s[nx][ny] == c) {
                    q.emplace(nx, ny);
                    used[nx][ny] = true;
                }
            }
        }
        res.push_back(std::move(side));
    }
    return make_tuple(start, goal, res);
}

auto dijkstra(int start, int goal, vector<vector<P>>& components) {
    if (start == goal) {
        return 0;
    }

    using S = pair<int, int>;  // -cost, node
    int m = components.size();

    const int inf = 1<<29;
    auto calc_dist = [&](int i, int j) {
        assert(i != j);
        int dist = inf;
        for (auto [x1, y1] : components[i]) {
            for (auto [x2, y2] : components[j]) {
                dist = min(dist, abs(x2 - x1) + abs(y2 - y1) - 1);
            }
        }
        return dist;
    };

    vector<int> dist(m, inf);
    priority_queue<S> q;
    dist[start] = 0;
    q.emplace(0, start);
    while (!q.empty()) {
        auto [cost, u] = q.top(); q.pop();
        cost = -cost;
        if (cost > dist[u]) continue;

        if (u == goal) {
            break;
        }
        for (int v = 0; v < m; v++) {
            if (u == v) continue;
            if (dist[v] <= dist[u]) continue;
            int d = calc_dist(u, v);
            int nd = cost + d;
            if (nd < dist[v]) {
                dist[v] = nd;
                q.emplace(-nd, v);
            }
        }
    }
    return dist[goal];
}

int solve(int n, char c, vector<string>& s) {
    auto [x, y, a] = bfs(n, c, s);
    int d = dijkstra(x, y, a);
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
