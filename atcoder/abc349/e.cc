#include <iostream>
#include <vector>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    vector<vector<long>> a(3, vector<long>(3));
    rep(i,3) rep(j,3) cin >> a[i][j];

    vector<vector<int>> b(3, vector<int>(3, -1));

    const int takahashi = 0;
    const int aoki = 1;
    auto winner = [&] {
        for (int i = 0; i < 3; i++) {
            if (b[i][0] != -1 && b[i][0] == b[i][1] && b[i][1] == b[i][2]) {
                return b[i][0];
            }
        }
        for (int j = 0; j < 3; j++) {
            if (b[0][j] != -1 && b[0][j] == b[1][j] && b[1][j] == b[2][j]) {
                return b[0][j];
            }
        }
        if (b[0][0] != -1 && b[0][0] == b[1][1] && b[1][1] == b[2][2]) {
            return b[0][0];
        }
        if (b[0][2] != -1 && b[0][2] == b[1][1] && b[1][1] == b[2][0]) {
            return b[0][2];
        }
        return -1;
    };

    auto dfs = [&](auto self, int player, int cnt) {
        {
            int w = winner();
            if (w != -1) return w;
        }
        if (cnt == 9) {
            long x = 0, y = 0;
            rep(i,3) rep(j,3) {
                if (b[i][j] == takahashi) x += a[i][j];
                else if (b[i][j] == aoki) y += a[i][j];
            }
            if (x > y) {
                return takahashi;
            }
            else {
                return aoki;
            }
        }
        int w = 1 - player;
        rep(i,3) rep(j,3) {
            if (b[i][j] != -1) continue;
            b[i][j] = player;
            int v = self(self, 1 - player, cnt + 1);
            if (v == player) {
                w = player;
            }
            b[i][j] = -1;
        }
        return w;
    };

    int w = dfs(dfs, takahashi, 0);
    if (w == takahashi) {
        cout << "Takahashi" << endl;
    }
    else {
        cout << "Aoki" << endl;
    }
}
