#include <iostream>
#include <vector>
#include <string>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, q;
    cin >> n >> q;

    vector<string> p(n);
    rep(i,n) cin >> p[i];

    vector<vector<int>> sum(n+1, vector<int>(n+1));
    rep(i,n) rep(j,n) {
        int x = (p[i][j] == 'B');
        sum[i+1][j+1] = sum[i+1][j] + sum[i][j+1] - sum[i][j] + x;
    }

    auto count = [&](int h, int w) {
        long x = h/n;
        long y = w/n;
        long rx = h%n;
        long ry = w%n;
        long ans = 0;
        ans += sum[n][n] * x * y;
        ans += sum[rx][n] * y;
        ans += sum[n][ry] * x;
        ans += sum[rx][ry];
        return ans;
    };

    rep(_,q) {
        int a, b, c, d;
        cin >> a >> b >> c >> d;
        c++; d++;
        long ans = count(c,d) - count(a,d) - count(c,b) + count(a,b);
        cout << ans << endl;
    }
}
