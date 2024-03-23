#include <iostream>
#include <iomanip>
#include <vector>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

template <class T>
inline void hash_combine(std::size_t& seed, const T& v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}


struct Board {
    size_t hash() const {
        size_t seed = 0;
        for (const auto& v : _board) for (const auto& x : v) hash_combine(seed, x);
        return seed;
    }
    void rotate(int x, int y) {
        size_t h = _board.size();
        size_t w = _board[0].size();
        for (int idx = 0; idx < (h -1)*(w - 1)/2; idx++) {
            int i = idx/(w-1);
            int j = idx%(w-1);
            swap(_board[x+i][y+j], _board[x+h-2-i][y+w-2-j]);
        }
    }
    void print() {
        size_t h = _board.size();
        size_t w = _board[0].size();
        rep(i,h) {
            rep(j,w) cerr << setw(2) << _board[i][j] << " ";
            cerr << endl;
        }
    }
    vector<vector<int>> _board;
};

constexpr int inf = 10000;

void dfs(Board& b, int depth, int px, int py, map<size_t, int>& mp) {
    size_t h = b.hash();
    if (mp.find(h) == mp.end()) mp[h] = inf;
    mp[h] = min(mp[h], depth);
    if (depth == 10) {
        return;
    }
    rep(x, 2) rep(y, 2) {
        if (x == px && y == py) continue;
        b.rotate(x, y);
        dfs(b, depth+1, x, y, mp);
        b.rotate(x, y);  // restore
    }
}

int dfs2(Board& b, int depth, int px, int py, const map<size_t, int>& mp) {
    size_t h = b.hash();
    if (mp.find(h) != mp.end()) {
        return mp.at(h) + depth;
    }
    if (depth == 10) {
        return inf;
    }
    int ans = inf;
    rep(x, 2) rep(y, 2) {
        if (x == px && y == py) continue;
        b.rotate(x, y);
        ans = min(ans, dfs2(b, depth+1, x, y, mp));
        b.rotate(x, y);  // restore
    }
    return ans;
}

int main() {
    int h, w;
    cin >> h >> w;

    vector<vector<int>> s(h, vector<int>(w));
    rep(i,h) rep(j,w) cin >> s[i][j];

    vector<vector<int>> g(h, vector<int>(w));
    rep(i,h) rep(j,w) g[i][j] = w*i + j + 1;

    Board b{s};
    Board goal{g};

    map<size_t, int> mp;
    dfs(b, 0, -1, -1, mp);
    int ans = dfs2(goal, 0, -1, -1, mp);

    if (ans > 20) {
        cout << -1 << endl;
    }
    else {
        cout << ans << endl;
    }
}
