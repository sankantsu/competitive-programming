#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <set>
#include <queue>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;


int main() {
    int h, w;
    cin >> h >> w;

    using S = vector<vector<int>>;
    S a(h, vector<int>(w)), b(h, vector<int>(w));
    rep(i,h) rep(j,w) cin >> a[i][j];
    rep(i,h) rep(j,w) cin >> b[i][j];

    auto print = [](const S& a) {
        for (auto row : a) {
            for (auto x : row) {
                cerr << x << " ";
            }
            cerr << endl;
        }
        cerr << endl;
    };

    auto swap_row = [](const S& a, int i, int j) {
        S res = a;
        swap(res[i], res[j]);
        return res;
    };

    auto swap_col = [h](const S& a, int i, int j) {
        S res = a;
        for (int k = 0; k < h; k++) swap(res[k][i], res[k][j]);
        return res;
    };

    int ans = -1;
    set<S> s;
    using P = pair<S, int>;
    queue<P> q;
    q.emplace(a, 0);
    while (!q.empty()) {
        auto [x, cnt] = q.front(); q.pop();
        /* print(x); */
        if (x == b) {
            ans = cnt;
            break;
        }
        rep(k, h-1) {
            auto y = swap_row(x, k, k+1);
            if (s.count(y)) continue;
            s.insert(y);
            q.emplace(y, cnt+1);
        }
        rep(k, w-1) {
            auto y = swap_col(x, k, k+1);
            if (s.count(y)) continue;
            s.insert(y);
            q.emplace(y, cnt+1);
        }
    }
    cout << ans << endl;
}
