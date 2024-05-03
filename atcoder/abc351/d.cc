#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <queue>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int h, w;
    cin >> h >> w;
    
    vector<string> s(h);
    rep(i,h) cin >> s[i];

    vector<vector<bool>> done(h, vector<bool>(w));
    vector<vector<int>> visited(h, vector<int>(w, -1));

    int dx[4] = {1, 0, -1, 0};
    int dy[4] = {0, 1, 0, -1};

    auto is_movable = [&](int i, int j) {
        bool movable = true;
        for (int dir = 0; dir < 4; dir++) {
            int ni = i + dx[dir];
            int nj = j + dy[dir];
            if (ni < 0 || h <= ni || nj < 0 || w <= nj) continue;
            if (s[ni][nj] == '#') movable = false;
        }
        return movable;
    };

    long ans = -1;
    using P = pair<int, int>;

    int bfs_cnt = 0;
    rep(si,h) rep(sj,w) {
        bfs_cnt++;
        if (done[si][sj]) continue;

        // skip magnet cell
        done[si][sj] = true;
        if (s[si][sj] == '#') continue;
        
        // bfs
        long cnt = 0;
        queue<P> q;
        visited[si][sj] = bfs_cnt;
        cnt++;
        q.emplace(si, sj);
        while(!q.empty()) {
            auto [i,j] = q.front(); q.pop();
            if (is_movable(i, j)) {
                for (int dir = 0; dir < 4; dir++) {
                    int ni = i + dx[dir];
                    int nj = j + dy[dir];
                    if (ni < 0 || h <= ni || nj < 0 || w <= nj) continue;
                    if (s[ni][nj] == '#' || visited[ni][nj] == bfs_cnt) continue;
                    visited[ni][nj] = bfs_cnt;
                    cnt++;
                    q.emplace(ni, nj);

                    // can move both direction
                    if (is_movable(ni, nj)) {
                        done[ni][nj] = true;
                    }
                }
            }
        }
        ans = max(ans, cnt);
    }
    cout << ans << endl;
}
