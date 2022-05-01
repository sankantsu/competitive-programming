// D - Grid Repainting
// https://atcoder.jp/contests/abc088/tasks/abc088_d
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    // input
    int h,w;
    cin >> h >> w;
    vector<string> s(h);
    rep(i,h) cin >> s[i];
    // bfs
    int dx[] = {-1,0,1,0};
    int dy[] = {0,1,0,-1};
    const int inf = 1<<20;
    queue<pair<int,int>> q;
    vector<vector<int>> dist(h,vector<int>(w,inf));
    q.emplace(0,0);
    dist[0][0] = 0;
    while(!q.empty()) {
        auto [i,j] = q.front(); q.pop();
        int d = dist[i][j];
        auto check = [h,w,&s](int i, int j) {
            return 0 <= i && i < h && 0 <= j && j < w && s[i][j] == '.';
        };
        rep(dir,4) {
            int ni = i + dx[dir];
            int nj = j + dy[dir];
            if (!check(ni,nj)) continue;
            if (dist[ni][nj] < inf) continue;
            dist[ni][nj] = d+1;
            q.emplace(ni,nj);
        }
    }
    // output
    {
        int d = dist[h-1][w-1]+1;
        if (d > inf) {
            cout << -1 << endl;
        }
        else {
            int cnt = 0;
            rep(i,h) rep(j,w) if (s[i][j] == '.') cnt++;
            int ans = cnt - d;
            cout << ans << endl;
        }
    }
}
