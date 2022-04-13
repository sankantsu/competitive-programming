// cheese
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int h,w,n;
char field[2000][2000];

constexpr int inf = 1<<30;
int x[10];
int y[10];
int dist[2000][2000];

int dx[4] = {1,0,-1,0};
int dy[4] = {0,-1,0,1};

int bfs(int s, int t) {
    using pos = pair<int,int>;
    rep(i,h) rep(j,w) {
        dist[i][j] = inf;
    }
    queue<pos> q;
    q.emplace(x[s],y[s]);
    dist[x[s]][y[s]] = 0;
    while (!q.empty()) {
        auto [i,j] = q.front(); q.pop();
        if (i == x[t] && j == y[t]) {
            break;
        }
        int d = dist[i][j];
        for (int dir = 0; dir < 4; dir++) {
            int ni = i + dx[dir];
            int nj = j + dy[dir];
            int nd = d+1;
            auto check_range = [](int i, int j) {
                return 0 <= i && i < h && 0 <= j && j < w;
            };
            if (!check_range(ni,nj)) continue;
            if (field[ni][nj] == 'X') continue;
            if (nd < dist[ni][nj]) {
                dist[ni][nj] = nd;
                q.emplace(ni,nj);
            }
        }
    }
    return dist[x[t]][y[t]];
}

int main() {
    cin >> h >> w >> n;
    rep(i,h) rep(j,w) {
        char c;
        cin >> c;
        field[i][j] = c;
        if (c == 'S' || ('1' <= c && c <= '9')) {
            int k = (c == 'S') ? 0 : c-'0';
            x[k] = i;
            y[k] = j;
        }
    }
    long res = 0;
    rep(i,n) {
        res += bfs(i,i+1);
    }
    cout << res << endl;
}
