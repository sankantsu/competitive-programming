// D - Restoring Road Network
// https://atcoder.jp/contests/abc074/tasks/arc083_b
#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 300;

int n;
long a[max_n+10][max_n+10];

long b[max_n+10][max_n+10];

int main() {
    cin >> n;
    rep(i,n) rep(j,n) cin >> a[i][j];

    using road = tuple<long,int,int>;
    priority_queue<road,vector<road>,greater<road>> q;
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            q.emplace(a[i][j],i,j);
        }
    }

    const long inf = 1L<<60;
    rep(i,n) rep(j,n) {
        if (i == j) b[i][j] = 0;
        else b[i][j] = inf;
    }

    long ans = 0;
    while(!q.empty()) {
        auto [c,i,j] = q.top(); q.pop();
        if (c == b[i][j]) continue;
        else if (c > b[i][j]) {
            ans = -1;
            break;
        }
        else {
            ans += c;
            b[i][j] = c;
            b[j][i] = c;
            rep(k,n) rep(l,n) {
                b[k][l] = min(b[k][l],b[k][i]+b[i][j]+b[j][l]);
                b[k][l] = min(b[k][l],b[k][j]+b[j][i]+b[i][l]);
            }
        }
    }
    cout << ans << endl;
}
