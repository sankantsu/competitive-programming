// Amazing Mazes (迷図と命ず）
// https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1166&lang=jp
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <bitset>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int w,h;
int vwall[50][50]; // 縦の壁
int hwall[50][50]; // 横の壁
int wall[50][50][4]; // 各マス(0,0)-(h-1,w-1)について右下左上方向それぞれに壁があるかないか

int dx[] = {0,1,0,-1};
int dy[] = {1,0,-1,0};

using pos = pair<int,int>;

const int inf = 1<<30;
int dist[50][50];

void bfs() {
    rep(i,h) rep(j,w) dist[i][j] = inf;
    dist[0][0] = 1;
    queue<pos> q;
    q.emplace(0,0);
    while (!q.empty()) {
        auto [i,j] = q.front(); q.pop();
        if (i == h-1 && j == w-1) break;
        int d = dist[i][j];
        rep(dir,4) {
            if (wall[i][j][dir]) continue;
            int ni = i+dx[dir];
            int nj = j+dy[dir];
            int nd = d+1;
            if (dist[ni][nj] < inf) continue;
            dist[ni][nj] = nd;
            q.emplace(ni,nj);
        }
    }
}

bool solve() {
    cin >> w >> h;
    if (w == 0 && h == 0) return false;
    rep(i,h) {
        rep(j,w-1) cin >> vwall[i][j];
        if (i != h-1) rep(j,w) cin >> hwall[i][j];
    }
    rep(i,h) rep(j,w) {
        wall[i][j][0] = (j == w-1) ? 1 : vwall[i][j]; // 右の壁
        wall[i][j][1] = (i == h-1) ? 1 : hwall[i][j]; // 下の壁
        wall[i][j][2] = (j == 0) ? 1 : vwall[i][j-1]; // 左の壁
        wall[i][j][3] = (i == 0) ? 1 : hwall[i-1][j]; // 上の壁
    }
    bfs();
    if (dist[h-1][w-1] == inf) dist[h-1][w-1] = 0;
    cout << dist[h-1][w-1] << endl;
    return true;
}

int main() {
    while (solve());
    return 0;
}
