#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n, m;
int g[12][12];

int pop_count(int x) {
    int cnt = 0;
    while (x > 0) {
        if (x&1) cnt++;
        x >>= 1;
    }
    return cnt;
}

bool is_clique(int i) {
    for (int j = 0; j < n; j++) {
        if (!((i>>j)&1)) continue;
        for (int k = j+1; k < n; k++) {
            if (!((i>>k)&1)) continue;
            if (g[j][k] == 0) {
                return false;
            }
        }
    }
    return true;
}

int main() {
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        int x,y;
        cin >> x >> y; x--; y--;
        g[x][y] = 1;
        g[y][x] = 1;
    }

    int res = -1;
    for (int i = 0; i < (1<<n); i++) {
        if (is_clique(i)) {
            res = max(res,pop_count(i));
        }
    }
    cout << res << endl;
}
