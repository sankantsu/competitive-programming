#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int pow3(int n) {
    if (n == 0) {
        return 1;
    } else {
        return 3 * pow3(n - 1);
    }
}

vector<vector<char>> level(int n) {
    int w = pow3(n);
    vector<vector<char>> v(w, vector<char>(w, '.'));
    if (n == 0) {
        v[0][0] = '#';
    } else {
        auto prev = level(n - 1);
        int pw = w/3;
        rep(i, 3) rep(j, 3) {
            if (i == 1 && j == 1) continue;
            rep(x, pw) rep(y, pw) {
                v[pw*i + x][pw*j + y] = prev[x][y];
            }
        }
    }
    return v;
}

int main() {
    int n;
    cin >> n;

    int w = pow3(n);
    auto v = level(n);
    rep(i,w) {
        rep(j,w) {
            cout << v[i][j];
        }
        cout << endl;
    }
}
