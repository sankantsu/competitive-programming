// D - 薄氷渡り
// https://atcoder.jp/contests/joi2009yo/tasks/joi2009yo_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
int ice[100][100];

int dx[4] = {-1,0,1,0};
int dy[4] = {0,1,0,-1};

bool used[100][100];

int dfs(int i, int j) {
    used[i][j] = true;
    auto check = [](int i, int j) {
        return 0 <= i && i < n && 0 <= j && j < m && ice[i][j] == 1;
    };
    int res = 1;
    rep(dir,4) {
        int ni = i + dx[dir];
        int nj = j + dy[dir];
        if (!check(ni,nj)) continue;
        if (used[ni][nj]) continue;
        int d = dfs(ni,nj);
        res = max(res,d+1);
    }
    used[i][j] = false;
    return res;
}

int main() {
    cin >> m >> n;
    rep(i,n) rep(j,m) cin >> ice[i][j];

    int res = -1;
    rep(i,n) rep(j,m) {
        if (ice[i][j] == 0) continue;
        int d = dfs(i,j);
        /* cerr << "i,j,d: " << i << " " << j << " " << d << endl; */
        res = max(res,d);
    }
    cout << res << endl;
}
