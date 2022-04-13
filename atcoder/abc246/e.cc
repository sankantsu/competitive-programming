// TLE
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

constexpr int inf = 1500*1500+1;

int n;
int ax,ay,bx,by;
char field[1500][1500];

int dist[1500][1500];

using point = pair<int,int>;
queue<pair<int,point>> q;

int dx[4] = {1,1,-1,-1};
int dy[4] = {1,-1,-1,1};

void bfs() {
    while (!q.empty()) {
        auto [d,p] = q.front(); q.pop();
        auto [i,j] = p;
        /* cout << "bfs: " << d << " " << i << " " << j << endl; */
        if (i == bx && j == by) {
            dist[bx][by] = d;
            break;
        }
        if (d < dist[i][j]) {
            dist[i][j] = d;
            for (int dir = 0; dir < 4; dir++) {
                for (int l = 1; l < n; l++) {
                    int ii = i + l*dx[dir];
                    int jj = j + l*dy[dir];
                    if (ii < 0 || n <= ii || jj < 0 || n <= jj) continue;
                    if (field[ii][jj] == '#') break;
                    q.emplace(d+1,make_pair(ii,jj));
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
        bfs();
        if (dist[bx][by] == inf) dist[bx][by] = -1;
        cout << dist[bx][by] << endl;
    }
}
