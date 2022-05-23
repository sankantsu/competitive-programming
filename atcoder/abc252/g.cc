#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int mod = 998244353;

int n;
int p[510];

long dp[510][510];
long dp2[510][510];

long rec(int l, int r);
long rec2(int l, int r);

long rec(int l, int r) {
    assert(l < r);
    if (dp[l][r] != -1) {
        return dp[l][r];
    }
    if (r - l == 1) {
        dp[l][r] = 1;
    }
    else {
        long res = 0;
        for (int k = l+1; k < r; k++) {
            res = (res + rec2(l,k)*rec(k,r))%mod;
        }
        dp[l][r] = res;
    }
    return dp[l][r];
}

long rec2(int l, int r) {
    assert(l < r);
    if (dp2[l][r] != -1) {
        return dp2[l][r];
    }
    if (r - l == 1) {
        dp2[l][r] = 1;
    }
    else {
        long res = 0;
        for (int k = l+1; k < r; k++) {
            if (p[r] > p[k]) {
                res = (res + rec2(l,k)*rec(k,r))%mod;
            }
        }
        dp2[l][r] = res;
    }
    return dp2[l][r];
}

int main() {
    cin >> n;
    rep(i,n) {
        cin >> p[i];
    }
    rep(i,n+1) rep(j,n+1) {
        dp[i][j] = -1;
        dp2[i][j] = -1;
    }
    long res = rec(0,n);
    cout << res << endl;
}
