#include <iostream>
#include <vector>
#include <random>
#include <chrono>
#include <array>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int on_line[301][301][301];

int main() {
    long n;
    cin >> n;

    vector<long> x(n), y(n);
    rep(i,n) cin >> x[i] >> y[i];

    vector<long> cnt(n);
    rep(i,n) rep(j,n) rep(k,n) {
        long dx1 = x[j] - x[i];
        long dy1 = y[j] - y[i];
        long dx2 = x[k] - x[i];
        long dy2 = y[k] - y[i];
        if (dx1*dy2 - dx2*dy1 == 0) {
            on_line[i][j][k] = 1;
        }
    }

    using T = array<long, 3>;
    vector<T> vs;

    int score = 0;
    rep(i,n/3) {
        vs.push_back({3*i, 3*i+1, 3*i+2});
        if (!on_line[3*i][3*i+1][3*i+2]) score++;
    }
    if (n%3 == 1) {
        vs.push_back({n-1, -1, -1});
    }
    if (n%3 == 2) {
        vs.push_back({n-2, n-1, -1});
    }
    int m = vs.size();

    if (score == n/3) {
        cout << score << endl;
        return 0;
    }

    mt19937 mt;
    auto start = chrono::steady_clock::now();
    while (true) {
        auto now = chrono::steady_clock::now();
        auto elapsed = chrono::duration_cast<chrono::milliseconds>(now - start).count();
        if (elapsed > 1500) {
            break;
        }
        int u = mt()%m;
        int v = mt()%m;
        if (u == v) continue;
        auto& t1 = vs[u];
        auto& t2 = vs[v];
        int nd = on_line[t1[0]][t1[1]][t1[2]] + on_line[t2[0]][t2[1]][t2[2]];
        if (nd == 0) continue;

        int r1 = mt()%3;
        int r2 = mt()%3;
        if (vs[u][r1] == -1 || vs[v][r2] == -1) continue;

        swap(vs[u][r1], vs[v][r2]);
        int nd2 = on_line[t1[0]][t1[1]][t1[2]] + on_line[t2[0]][t2[1]][t2[2]];
        if (nd2 < nd) {
            score = score + (nd - nd2);
            if (score == n/3) break;
        }
        else {
            swap(vs[u][r1], vs[v][r2]);
        }
    }
    cout << score << endl;
}
