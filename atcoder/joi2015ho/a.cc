// A - 鉄道旅行 (Railroad Trip)
// https://atcoder.jp/contests/joi2015ho/tasks/joi2015ho_a
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 100000;
int n,m;
int p[max_n+1];
long a[max_n+1];
long b[max_n+1];
long c[max_n+1];

long cnt[max_n+2];
long sum[max_n+2];

int main() {
    cin >> n >> m;
    rep(i,m) {
        cin >> p[i];
        p[i]--;
    }
    rep(i,n-1) cin >> a[i] >> b[i] >> c[i];

    rep(i,m-1) {
        int l = min(p[i],p[i+1]);
        int r = max(p[i],p[i+1]);
        cnt[l]++;
        cnt[r]--;
    }
    rep(i,n-1) {
        sum[i+1] = sum[i]+cnt[i];
    }
    long res = 0;
    rep(i,n-1) {
        long c1 = a[i]*sum[i+1];
        long c2 = b[i]*sum[i+1] + c[i];
        res += min(c1,c2);
    }
    cout << res << endl;
}
