// TLE, RE?
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
queue<tuple<int,int,point>> q; // dist, dir, pos

int dx[4] = {1,1,-1,-1};
int dy[4] = {1,-1,-1,1};

void bfs() {
    while (!q.empty()) {
        auto [d,k,p] = q.front(); q.pop();
        auto [i,j] = p;
        cout << "bfs: " << d << " " << k << " " << i << " " << j << endl;
        if (i == bx && j == by) {
            dist[bx][by] = d;
            return;
        }
        if (d < dist[i][j] || d == 0) {
            dist[i][j] = d;
            for (int dir = 1-k; dir < 4; dir += 2) {
                for (int l = 1; l < n; l++) {
                    int ii = i + l*dx[dir];
                    int jj = j + l*dy[dir];
                    if (ii < 0 || n <= ii || jj < 0 || n <= jj) break;
                    if (field[ii][jj] == '#') break;
                    if (dist[ii][jj] == inf) {
                        q.emplace(d+1,1-k,make_pair(ii,jj));
                    }
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
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cout << field[i][j];
        }cout << endl;
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
        q.emplace(0,0,make_pair(ax,ay));
        q.emplace(0,1,make_pair(ax,ay));
        bfs();
        if (dist[bx][by] == inf) dist[bx][by] = -1;
        cout << dist[bx][by] << endl;
    }
}
