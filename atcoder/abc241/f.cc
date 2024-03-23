#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <queue>
#include <map>
#include <random>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

using P = pair<long, long>;

long solve(long h, long w, long n, long sx, long sy, long gx, long gy, const vector<P>& points) {
    map<long, vector<long>> xindex;
    map<long, vector<long>> yindex;
    rep(i,n) {
        auto [x,y] = points[i];
        xindex[x].push_back(y);
        yindex[y].push_back(x);
    }
    for (auto& [x,vy] : xindex) sort(vy.begin(), vy.end());
    for (auto& [y,vx] : yindex) sort(vx.begin(), vx.end());

    // bfs
    constexpr long inf = 1L<<60;
    map<long, map<long, size_t>> dist;
    queue<P> q;
    q.emplace(sx, sy);
    dist[sx][sy] = 0;
    while (!q.empty()) {
        auto [x, y] = q.front();
        /* cerr << "x,y: " << x << " " << y << endl; */
        q.pop();
        if (x == gx && y == gy) {
            break;
        }
        if (xindex.contains(x)) {
            auto& vy = xindex[x];
            auto it = lower_bound(vy.begin(), vy.end(), y);
            if (it != vy.end() && !dist[x].contains(*it-1)) {
                q.emplace(x, *it-1);
                dist[x][*it-1] = dist[x][y] + 1;
            }
            if (it != vy.begin() && !dist[x].contains(*prev(it)+1)) {
                q.emplace(x, *prev(it)+1);
                dist[x][*prev(it)+1] = dist[x][y] + 1;
            }
        }
        if (yindex.contains(y)) {
            auto& vx = yindex[y];
            auto it = lower_bound(vx.begin(), vx.end(), x);
            if (it != vx.end() && !dist[*it-1].contains(y)) {
                q.emplace(*it-1, y);
                dist[*it-1][y] = dist[x][y] + 1;
            }
            if (it != vx.begin() && !dist[*prev(it)+1].contains(y)) {
                q.emplace(*prev(it)+1, y);
                dist[*prev(it)+1][y] = dist[x][y] + 1;
            }
        }
    }

    if (!dist[gx].contains(gy)) {
        return -1;
    }
    else {
        return dist[gx][gy];
    }
}

long solve_jury(long h, long w, long n, long sx, long sy, long gx, long gy, const vector<P>& points) {
    vector<vector<int>> field(h+2, vector<int>(w+2));
    rep(i,n) {
        auto [x,y] = points[i];
        field[x][y] = 1;
    }
    /* for (long i = 1; i <= h; i++) { */
    /*     for (long j = 1; j <= w; j++){ */
    /*         char c; */
    /*         if (i == sx && j == sy) c = 's'; */
    /*         else if (i == gx && j == gy) c = 'g'; */
    /*         else if (field[i][j]) c = '#'; */
    /*         else c = '.'; */
    /*         cerr << c; */
    /*     } */
    /*     cerr << endl; */
    /* } */

    constexpr long inf = 1L<<60;
    vector<vector<long>> dist(h+2, vector<long>(w+2, inf));
    queue<P> q;
    q.emplace(sx, sy);
    dist[sx][sy] = 0;
    while(!q.empty()) {
        auto [x,y] = q.front();
        q.pop();
        if (x == gx && y == gy) break;
        {
            long nx = x;
            long ny = y;
            while (true) {
                if (field[nx+1][ny] == 1) {
                    break;
                }
                nx = nx + 1;
                if (nx == 0 || nx == h+1) {
                    nx = inf;
                    break;
                }
            }
            /* cerr << "nx,ny: " << nx << " " << ny << endl; */
            if (nx != inf && dist[nx][ny] == inf) {
                q.emplace(nx, ny);
                dist[nx][ny] = dist[x][y] + 1;
            }
        }
        {
            long nx = x;
            long ny = y;
            while (true) {
                if (field[nx-1][ny] == 1) {
                    break;
                }
                nx = nx - 1;
                if (nx == 0 || nx == h+1) {
                    nx = inf;
                    break;
                }
            }
            if (nx != inf && dist[nx][ny] == inf) {
                q.emplace(nx, ny);
                dist[nx][ny] = dist[x][y] + 1;
            }
        }
        {
            long nx = x;
            long ny = y;
            while (true) {
                if (field[nx][ny+1] == 1) {
                    break;
                }
                ny = ny + 1;
                if (ny == 0 || ny == w+1) {
                    ny = inf;
                    break;
                }
            }
            if (ny != inf && dist[nx][ny] == inf) {
                q.emplace(nx, ny);
                dist[nx][ny] = dist[x][y] + 1;
            }
        }
        {
            long nx = x;
            long ny = y;
            while (true) {
                if (field[nx][ny-1] == 1) {
                    break;
                }
                ny = ny - 1;
                if (ny == 0 || ny == w+1) {
                    ny = inf;
                    break;
                }
            }
            if (ny != inf && dist[nx][ny] == inf) {
                q.emplace(nx, ny);
                dist[nx][ny] = dist[x][y] + 1;
            }
        }
    }
    if (dist[gx][gy] == inf) {
        return -1;
    }
    else {
        return dist[gx][gy];
    }
}

auto gen() {
    static mt19937 mt;

    long h = 7;
    long w = 7;
    long n = 7;
    
    long sx = 1 + (mt()%h);
    long sy = 1 + (mt()%w);

    long gx = 1 + (mt()%h);
    long gy = 1 + (mt()%w);
    while (gx == sx && gy == sy) {
        gx = 1 + (mt()%h);
        gy = 1 + (mt()%w);
    }

    vector<P> points;
    /* map<P, size_t> cnt; */
    rep(i,n) {
        long x = 1 + (mt()%h);
        long y = 1 + (mt()%w);
        while ((x == sx && y == sy) || (x == gx && y == gy)) {
            x = 1 + (mt()%h);
            y = 1 + (mt()%w);
        }
        points.emplace_back(x,y);
    }

    return make_tuple(h,w,n,sx,sy,gx,gy,points);
}

void test() {
    long n_cases = 10000;
    rep(i,n_cases) {
        auto [h,w,n,sx,sy,gx,gy,points] = gen();
        long ans = solve(h, w, n, sx, sy, gx, gy, points);
        long ans_jury = solve_jury(h, w, n, sx, sy, gx, gy, points);
        if (ans != ans_jury) {
            cerr << "Wrong answer!!" << endl;
            cerr << "expected: " << ans_jury << endl;
            cerr << "actual: " << ans << endl;
            cout << h << " " << w << " " << n << endl;
            cout << sx << " " << sy << endl;
            cout << gx << " " << gy << endl;
            rep(i,n) {
                auto [x,y] = points[i];
                cout << x << " " << y << endl;
            }
            break;
        }
    }
    cerr << "All test passed!!" << endl;
}

int main() {
    /* test(); */
    /* return 0; */

    long h, w, n;
    cin >> h >> w >> n;

    long sx, sy;
    cin >> sx >> sy;

    long gx, gy;
    cin >> gx >> gy;

    vector<P> points(n);
    rep(i,n) {
        long x, y;
        cin >> x >> y;
        points[i] = P{x, y};
    }

    long ans = solve(h, w, n, sx, sy, gx, gy, points);
    /* long ans = solve_jury(h, w, n, sx, sy, gx, gy, points); */
    cout << ans << endl;
}
