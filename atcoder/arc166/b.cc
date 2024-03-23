#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long cnt[200001][2][2][2];  // i, x|a? x|b? x|c? -> cnt
long dp[200001][2][2][2];  // until i, x|a? x|b? x|c? -> cnt

long gcd(long a, long b) {
    if (b == 0) return a;
    return gcd(b, a%b);
}

long lcm(long a, long b) {
    return a*b/gcd(a,b);
}

int main() {
    long n, a, b, c;
    cin >> n >> a >> b >> c;

    vector<long> A(n);
    rep(i,n) cin >> A[i];

    constexpr long inf = 1L<<61;
    rep(i,n) rep(x,2) rep(y,2) rep(z,2) {
        long k = 1;
        if (x) k = lcm(k,a);
        if (y) k = lcm(k,b);
        if (z) k = lcm(k,c);
        long m = A[i]/k*k;
        if (A[i] == m) cnt[i][x][y][z] = 0;
        else cnt[i][x][y][z] = m + k - A[i];
        /* cerr << "i,x,y,z,k,cnt: " << i << " " << x << " " << y << " " << z << " " << k << " " << cnt[i][x][y][z] << endl; */
    }
    rep(i,n) rep(x,2) rep(y,2) rep(z,2) dp[i][x][y][z] = inf;
    rep(x,2) rep(y,2) rep(z,2) dp[0][x][y][z] = cnt[0][x][y][z];
    rep(i,n-1) rep(x,2) rep(y,2) rep(z,2) rep(x2,2) rep(y2,2) rep(z2,2) {
        long nx = max(x,x2);
        long ny = max(y,y2);
        long nz = max(z,z2);
        dp[i+1][nx][ny][nz] = min(dp[i+1][nx][ny][nz], dp[i][x][y][z] + cnt[i+1][x2][y2][z2]);
    }
    /* rep(i,n) rep(x,2) rep(y,2) rep(z,2) { */
    /*     cerr << "i,x,y,z,dp: " << i << " " << x << " " << y << " " << z << " " << dp[i][x][y][z] << endl; */
    /* } */
    cout << dp[n-1][1][1][1] << endl;
}
