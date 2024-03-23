#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long t;
    cin >> t;

    rep(_,t) {
        vector<long> a(5), p(5);
        rep(i,5) cin >> a[i];
        rep(i,5) cin >> p[i];

        long n = 0;
        rep(i,5) n += a[i];

        long s = 0;
        rep(i,5) s += (i+1)*a[i];

        long k = 3*n - s;
        if (k <= 0) {
            cout << 0 << endl;
            continue;
        }

        long p1 = p[3];
        long p2 = p[4];

        long ans = 1L<<62;
        ans = min(ans, p1*k); // 4 のみ
        ans = min(ans, p2*((k+1)/2));  // 5 のみ
        ans = min(ans, p1 + p2*(k/2));  // 4 を 1 つだけ
        cout << ans << endl;
    }
}
