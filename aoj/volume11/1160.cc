#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int w,h;
int c[50][50];

int used[50][50];
int cnt;

/* int dx[4] = {1,0,-1,0}; */
/* int dy[4] = {0,-1,0,1}; */
int dx[8] = {1,1,1,0,-1,-1,-1,0};
int dy[8] = {1,0,-1,-1,-1,0,1,1};

void dfs(int i, int j) {
    /* cout << "dfs " << i << " " << j << endl; */
    used[i][j] = true;
    for (int d = 0; d < 8; d++) {
        int ni = i + dx[d];
        int nj = j + dy[d];
        if (ni < 0 || h <= ni || nj < 0 || w <= nj) continue;
        if (c[ni][nj] == 0) continue;
        if (used[ni][nj]) continue;
        dfs(ni,nj);
    }
}

bool solve() {
    cin >> w >> h;
    if (w == 0 && h == 0) {
        return false;
    }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            cin >> c[i][j];
        }
    }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            used[i][j] = false;
        }
    }
    cnt = 0;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            if (c[i][j] == 1 && !used[i][j]) {
                /* cout << "i,j: " << i << " " << j << endl; */
                dfs(i,j);
                cnt++;
            }
        }
    }
    cout << cnt << endl;
    return true;
}

int main() {
    while (solve());
    return 0;
}
