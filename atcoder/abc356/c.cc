#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, m, k;
    cin >> n >> m >> k;

    vector<long> c(m);
    vector<long> a(m);
    vector<bool> r(m);
    rep(i,m) {
        cin >> c[i];
        rep(j, c[i]) {
            long x;
            cin >> x; x--;
            a[i] |= 1<<x;
        }
        char _r;
        cin >> _r;
        if (_r == 'o') {
            r[i] = true;
        } else {
            r[i] = false;
        }
    }

    long ans = 0;
    for (int s = 0; s < 1<<n; s++) {
        bool ok = true;
        rep(i, m) {
            long cnt = 0;
            rep(j, n) {
                if ((a[i] & 1<<j) & s) {
                    cnt += 1;
                }
            }
            bool expect = (cnt >= k);
            if (expect != r[i]) {
                ok = false;
            }
        }
        if (ok) ans++;
    }
    cout << ans << endl;
}
