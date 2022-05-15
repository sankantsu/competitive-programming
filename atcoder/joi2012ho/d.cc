// D - 釘 (Nails)
// https://atcoder.jp/contests/joi2012ho/tasks/joi2012ho4

#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 5000;
const long max_m = 500000;

int n,m;
int a[max_m+10];
int b[max_m+10];
int x[max_m+10];

int cnt1[max_n+10][max_n+10];
int cnt2[max_n+10][max_n+10];
int sum[max_n+10][max_n+10];

int main() {
    cin >> n >> m;
    rep(i,m) {
        cin >> a[i] >> b[i] >> x[i];
    }

    rep(i,m) {
        cnt1[a[i]][b[i]]++;
        cnt1[a[i]+x[i]+1][b[i]]--;
        cnt2[a[i]][b[i]+1]--;
        cnt2[a[i]+x[i]+1][b[i]+x[i]+2]++;
    }
    rep(i,n+1) rep(j,n+1) {
        cnt1[i+1][j] = cnt1[i][j]+cnt1[i+1][j];
    }
    rep(i,n+1) rep(j,n+1) {
        cnt2[i+1][j+1] = cnt2[i][j]+cnt2[i+1][j+1];
    }
    rep(i,n+1) rep(j,n+1) {
        sum[i][j+1] = sum[i][j]+cnt1[i][j+1]+cnt2[i][j+1];
    }
    long res = 0;
    rep(i,n+1) rep(j,n+1) {
        if (sum[i][j] > 0) res++;
    }
    cout << res << endl;
}
