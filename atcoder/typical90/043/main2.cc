#include <iostream>
#include <queue>

using namespace std;

constexpr int max_h = 1000;
constexpr int max_w = 1000;
constexpr int inf = max_h*max_w + 1;

constexpr pair<int,int> dirs[4] = {{1,0},{0,-1},{-1,0},{0,1}};

int h,w;
int rs,cs,rt,ct;
int s[max_h][max_w];

int turn[max_h][max_w][4];

struct st {
    int i;
    int j;
    int dir;
    int cnt;
};

queue<st> que;

void bfs() {
    while (!que.empty()) {
        st state = que.front(); que.pop();
        int i = state.i;
        int j = state.j;
        int dir = state.dir;
        int cnt = state.cnt;
        bool flag = false;
        for (int d = 0; d <= 3; d++) {
            if (turn[i][j][d] < cnt) {
                flag = true;
            }
            if (turn[i][j][d] == cnt && d == dir) {
                flag = true;
            }
        }
        if (flag) continue;
        turn[i][j][dir] = cnt;
        for (int d = 0; d <= 3; d++) {
            if (d == 2) continue;
            int nextdir = (dir + d)%4;
            int nexti = i + dirs[nextdir].first;
            int nextj = j + dirs[nextdir].second;
            if (nexti < 0 || h <= nexti || nextj < 0 || w <= nextj) continue;
            if (s[nexti][nextj]) continue;
            if (nextdir == dir) {
                que.push(st{nexti,nextj,nextdir,cnt});
            }
            else {
                que.push(st{nexti,nextj,nextdir,cnt+1});
            }
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
        que.push(st{rs,cs,dir,0});
    }
    bfs();

    int ans = inf;
    for (int d = 0; d <= 3; d++) {
        ans = min(ans,turn[rt][ct][d]);
    }
    cout << ans << endl;
}
