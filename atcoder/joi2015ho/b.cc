#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 2000;

long n;
long a[max_n+10];

long dp[max_n+10][max_n+10];

long rec_joi(long l, long r);
long rec_ioi(long l, long r);

// pick larger one
long rec_joi(long l, long r) {
    if (dp[l][r] != -1) {
        return dp[l][r];
    }
    long s1 = a[l];
    long s2 = a[(r-1+n)%n];
    dp[l][r] = max(dp[l][r],s1+rec_ioi((l+1)%n,r));
    dp[l][r] = max(dp[l][r],s2+rec_ioi(l,(r-1+n)%n));
    return dp[l][r];
}

long rec_ioi(long l, long r) {
    if ((r-l+n)%n <= 1) {
        return 0;
    }
    long s1 = a[l];
    long s2 = a[(r-1+n)%n];
    if (s1 > s2) {
        return rec_joi((l+1)%n,r);
    }
    else {
        return rec_joi(l,(r-1+n)%n);
    }
}

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    rep(i,n) rep(j,n) {
        dp[i][j] = -1;
    }
    long res = -1;
    rep(i,n) {
        res = max(res,rec_joi(i,i));
    }
    cout << res << endl;
}
