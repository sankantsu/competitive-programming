// B - Buildings are Colorful!
// https://atcoder.jp/contests/s8pc-4/tasks/s8pc_4_b
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
long a[15];

long calc(unsigned s) {
    long b[15];
    rep(i,n) b[i] = a[i];
    long res = 0;
    long h = -1;
    for (int i = 1; i < n; i++) {
        h = max(h,b[i-1]);
        if ((s>>i)&1) {
            long diff = max(0L,h-b[i]+1);
            res += diff;
            b[i] += diff;
        }
    }
    return res;
}

int main() {
    cin >> n >> k;
    rep(i,n) cin >> a[i];
    unsigned comb = (1<<k)-1;
    long res = 1L<<60;
    while (comb < (1<<n)) {
        long r = calc(comb);
        res = min(res,r);
        unsigned x = comb&-comb;
        unsigned y = comb+x;
        unsigned z = comb&~y;
        comb = y | ((z/x)>>1);
    }
    cout << res << endl;
}
