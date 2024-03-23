#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long h, w;
    cin >> h >> w;
    
    vector<string> s(h);
    rep(i, h) cin >> s[i];

    long n_black = 0;
    rep(i,h) rep(j,w) if (s[i][j] == '#') n_black++;

    constexpr long inf = 1L<<60;
    vector<vector<long>> dist(h, vector<long>(w, inf));

    constexpr long delta[4][2] = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

    using P = pair<long, long>;
    queue<P> q;
    q.emplace(0, 0);
    dist[0][0] = 0;
    while(!q.empty()) {
        auto [i, j] = q.front();
        /* cerr << "i,j: " << i << " " << j << endl; */
        /* cerr << "dist: " << dist[i][j] << endl; */
        q.pop();
        if (i == h - 1 && j == w - 1) {
            break;
        }
        rep(dir, 4) {
            int ni = i + delta[dir][0];
            int nj = j + delta[dir][1];
            if (ni < 0 || h <= ni || nj < 0 || w <= nj) continue;
            /* cerr << "ni, nj: " << ni << " " << nj << endl; */
            if (s[ni][nj] == '#') continue;
            if (dist[ni][nj] < inf) continue;
            q.emplace(ni, nj);
            dist[ni][nj] = dist[i][j] + 1;
        }
    }
    if (dist[h-1][w-1] == inf) {
        cout << -1 << endl;
    }
    else {
        cout << h * w - dist[h-1][w-1] - n_black - 1 << endl;
    }
}
