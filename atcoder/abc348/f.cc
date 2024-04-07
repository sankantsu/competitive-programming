#include <iostream>
#include <vector>

#define rep(i,n) for (int i = 0; i < static_cast<int>((n)); i++)

using namespace std;

bool sim[2000][2000];

int main() {
    cin.tie(0);
    ios::sync_with_stdio(false);

    int n, m;
    cin >> n >> m;

    vector<vector<short>> a(n, vector<short>(m));
    rep(i,n) rep(j,m) cin >> a[i][j];

    vector<vector<short>> aT(m, vector<short>(n));
    rep(i,n) rep(j,m) aT[j][i] = a[i][j];

    for (int k = 0; k < m; k++) {
        for (int i = 0; i < n; i++) for (int j = i+1; j < n; j++) {
            sim[i][j] ^= (aT[k][i] == aT[k][j]);
        }
    }

    int ans = 0;
    rep(i,n) for (int j = 0; j < n; j++) ans += sim[i][j];
    cout << ans << endl;
}
