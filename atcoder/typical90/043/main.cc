#include <iostream>
#include <contest/util.h>

using namespace std;

constexpr int max_h = 1000;
constexpr int max_w = 1000;
constexpr int inf = max_h*max_w + 1;

constexpr pair<int,int> dirs[4] = {{1,0},{0,-1},{-1,0},{0,1}};

int h,w;
int rs,cs,rt,ct;
int s[max_h][max_w];

int turn[max_h][max_w][4];

// int count_dfs;

void dfs(int i, int j, int dir, int cnt) {
    // count_dfs++;
    print_vals("dfs",i,j,dir,cnt);
    for (int d = 0; d <= 3; d++) {
        if (turn[i][j][d] < cnt) {
            return;
        }
        if (turn[i][j][d] == cnt && d == dir) {
            return;
        }
    }
    turn[i][j][dir] = cnt;
    for (int d = 0; d <= 3; d++) {
        if (d == 2) continue;
        int nextdir = (dir + d)%4;
        int nexti = i + dirs[nextdir].first;
        int nextj = j + dirs[nextdir].second;
        if (nexti < 0 || h <= nexti || nextj < 0 || w <= nextj) continue;
        if (s[nexti][nextj]) continue;
        if (nextdir == dir) {
            dfs(nexti,nextj,nextdir,cnt);
        }
        else {
            dfs(nexti,nextj,nextdir,cnt+1);
        }
    }
}

int main() {
    cin >> h >> w;
    cin >> rs >> cs; rs--; cs--;
    cin >> rt >> ct; rt--; ct--;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            char c;
            cin >> c;
            if (c == '#') s[i][j] = 1;
        }
    }

    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            for (int d = 0; d <= 3; d++) {
                turn[i][j][d] = inf;
            }
        }
    }

    for (int dir = 0; dir <= 3; dir++) {
        dfs(rs,cs,dir,0);
    }

    // cout << "dfs count: " << count_dfs << endl;

    int ans = inf;
    for (int d = 0; d <= 3; d++) {
        ans = min(ans,turn[rt][ct][d]);
    }
    cout << ans << endl;
}
