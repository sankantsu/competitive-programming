#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

int r,c;
int s[2];
int g[2];
char maze[100][100];

int dx[] = {1,0,-1,0};
int dy[] = {0,-1,0,1};
int dist[100][100];

int main() {
    cin >> r >> c;
    cin >> s[0] >> s[1]; s[0]--; s[1]--;
    cin >> g[0] >> g[1]; g[0]--; g[1]--;
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            cin >> maze[i][j];
        }
    }

    const int inf = 100000;
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            dist[i][j] = inf;
        }
    }
    using pos = pair<int,int>;
    queue<pos> q;
    dist[s[0]][s[1]] = 0;
    q.push(make_pair(s[0],s[1]));
    while (!q.empty()) {
        auto [i,j] = q.front(); q.pop();
        int d = dist[i][j];
        for (int dir = 0; dir < 4; dir++) {
            int ni = i + dx[dir];
            int nj = j + dy[dir];
            if (ni < 0 || r <= ni || nj < 0 || c <= nj) continue;
            if (maze[ni][nj] == '#') continue;
            if (dist[ni][nj] == inf) {
                dist[ni][nj] = d+1;
                q.push(make_pair(ni,nj));
            }
        }
    }
    cout << dist[g[0]][g[1]] << endl;
}
