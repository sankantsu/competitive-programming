// TLE (1 case)
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <tuple>

using namespace std;

constexpr int inf = 1500*1500+1;

int n;
int ax,ay,bx,by;
char field[1500][1500];

int dist[1500][1500];

using point = pair<int,int>;
queue<tuple<int,point>> q; // dir, pos

int dx[4] = {1,1,-1,-1};
int dy[4] = {1,-1,-1,1};

void bfs() {
    while (!q.empty()) {
        auto [k,p] = q.front(); q.pop();
        auto [i,j] = p;
        /* cout << "bfs: " << d << " " << k << " " << i << " " << j << endl; */
        if (i == bx && j == by) {
            return;
        }
        int d = dist[i][j];
        for (int dir = 1-k; dir < 4; dir += 2) {
            for (int l = 1; l < n; l++) {
                int ii = i + l*dx[dir];
                int jj = j + l*dy[dir];
                int nd = d+1;
                if (ii < 0 || n <= ii || jj < 0 || n <= jj) break;
                if (field[ii][jj] == '#') break;
                if (nd < dist[ii][jj]) {
                    dist[ii][jj] = nd;
                    q.emplace(1-k,make_pair(ii,jj));
                }
            }
        }
    }
}

int main() {
    cin >> n;
    cin >> ax >> ay; ax--; ay--;
    cin >> bx >> by; bx--; by--;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> field[i][j];
        }
    }
    if ((ax+ay)%2 != ((bx+by)%2)) {
        cout << -1 << endl;
    }
    else {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                dist[i][j] = inf;
            }
        }
        q.emplace(0,make_pair(ax,ay));
        q.emplace(1,make_pair(ax,ay));
        dist[ax][ay] = 0;
        bfs();
        if (dist[bx][by] == inf) dist[bx][by] = -1;
        cout << dist[bx][by] << endl;
    }
}
