#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <cassert>
#include <atcoder/dsu.hpp>
#include <atcoder/modint.hpp>

using mint = atcoder::modint998244353;
using atcoder::dsu;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int h, w;
    cin >> h >> w;

    vector<string> s(h);
    rep(i,h) {
        cin >> s[i];
    }
    
    int dx[4] = {1, 0, -1, 0};
    int dy[4] = {0, 1, 0, -1};

    dsu uf(h*w);
    rep(i, h*w) {
        int x = i/w;
        int y = i%w;
        if (s[x][y] == '.') continue;

        for (int d = 0; d < 4; d++) {
            int nx = x + dx[d];
            int ny = y + dy[d];
            if (nx < 0 || h <= nx || ny < 0 || w <= ny) continue;
            int j = w*nx + ny;
            if (s[x][y] == '#' && s[nx][ny] == '#') {
                uf.merge(i, j);
            }
        }
    }

    int n = 0;
    auto groups = uf.groups();
    for (const auto& g : groups) {
        assert(!g.empty());
        int i = g[0];
        int x = i/w;
        int y = i%w;
        if (s[x][y] == '#') n++;
    }
    cerr << "n: " << n << endl;

    mint sum = 0;
    mint cnt = 0;
    rep(i, h*w) {
        int x = i/w;
        int y = i%w;
        if (s[x][y] == '#') continue;

        set<int> rs;
        for (int d = 0; d < 4; d++) {
            int nx = x + dx[d];
            int ny = y + dy[d];
            if (nx < 0 || h <= nx || ny < 0 || w <= ny) continue;
            int j = w*nx + ny;
            if (s[nx][ny] == '#') rs.insert(uf.leader(j));
        }
        sum += n - rs.size() + 1;
        cnt++;
    }
    
    mint ans = sum / cnt;
    cerr << "cnt: " << cnt.val() << endl;
    cerr << "sum: " << sum.val() << endl;
    cout << ans.val() << endl;
}
