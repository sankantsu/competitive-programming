#include <iostream>
#include <deque>

using namespace std;

constexpr int inf = 1500*1500+1;

int n;
int ax,ay,bx,by;
char field[1500][1500];
int dist[1500][1500][4];
int dx[4] = {1,1,-1,-1};
int dy[4] = {1,-1,-1,1};

using point = pair<int,int>;
using state = pair<point,int>; // pos, dir

deque<state> q;

int bfs() {
    while (!q.empty()) {
        auto [pos,dir] = q.front(); q.pop_front();
        auto [i,j] = pos;
        /* cout << i << " " << j << " " << dir << endl; */
        if (i == bx && j == by) {
            return dist[bx][by][dir];
        }
        int d = dist[i][j][dir];
        for (int ndir = 0; ndir < 4; ndir++) {
            int ii = i + dx[ndir];
            int jj = j + dy[ndir];
            int nd = (ndir == dir) ? d : d+1;
            if (ii < 0 || n <= ii || jj < 0 || n <= jj) continue;
            if (field[ii][jj] == '#') continue;
            if (nd < dist[ii][jj][ndir]) {
                dist[ii][jj][ndir] = nd;
                if (nd == d) {
                    q.emplace_front(make_pair(ii,jj),ndir);
                }
                else {
                    q.emplace_back(make_pair(ii,jj),ndir);
                }
            }
        }
    }
    return -1;
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
        return 0;
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            for (int dir = 0; dir < 4; dir++) {
                dist[i][j][dir] = inf;
            }
        }
    }
    for (int dir = 0; dir < 4; dir++) {
        dist[ax][ay][dir] = 1;
        q.emplace_back(make_pair(ax,ay),dir);
    }
    int res = bfs();
    cout << res << endl;
    return 0;
}
