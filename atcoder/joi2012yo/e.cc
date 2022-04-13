// E - Illumination
// https://atcoder.jp/contests/joi2012yo/tasks/joi2012yo_e
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int w,h;
int c[200][200];

bool used[200][200];

int dx[2][6] = {{1,0,-1,-1,-1,0},{1,1,0,-1,0,1}};
int dy[6] = {0,1,1,0,-1,-1};

int dfs(int x, int y) {
    /* cout << "dfs " << x << " " << y << endl; */
    used[x][y] = true;
    int cnt = 0;
    for (int dir = 0; dir < 6; dir++) {
        int nx = x + dx[y%2][dir];
        int ny = y + dy[dir];
        if (nx < 0 || w+1 < nx || ny < 0 || h+1 < ny) continue;
        if (used[nx][ny]) continue;
        if (c[nx][ny] == 1) {
            cnt++;
        }
        else {
            cnt += dfs(nx,ny);
        }
    }
    return cnt;
}

int main() {
    cin >> w >> h;
    rep(i,h) {
        rep(j,w) {
            cin >> c[j+1][i+1];
        }
    }
    int res = dfs(0,0);
    cout << res << endl;
}
