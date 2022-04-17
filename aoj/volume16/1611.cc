// Daruma Otoshi
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int w[300];

int dp[301][301];

int rec(int l, int r) {
    if (dp[l][r] >= 0) {
        return dp[l][r];
    }
    if (r - l <= 1) {
        dp[l][r] = 0;
    }
    else if (rec(l+1,r-1) == r-l-2 && abs(w[l]-w[r-1]) <= 1) {
        dp[l][r] = r-l;
    }
    else {
        int res = 0;
        for (int m = l+1; m < r; m++) {
            res = max(res,rec(l,m)+rec(m,r));
        }
        dp[l][r] = res;
    }
    return dp[l][r];
}

bool solve() {
    cin >> n;
    if (n == 0) return false;
    rep(i,n) cin >> w[i];
    rep(i,n+1) rep(j,n+1) dp[i][j] = -1;
    cout << rec(0,n) << endl;
    return true;
}

int main() {
    while (solve()) {}
    return 0;
}
