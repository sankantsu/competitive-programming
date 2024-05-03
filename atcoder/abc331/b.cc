#include <iostream>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, s, m, l;
    cin >> n >> s >> m >> l;

    long ans = (s + m + l) * 100;
    rep(i, 100) rep(j, 100) rep(k, 100) {
        if (6*i + 8*j + 12*k >= n) {
            long v = s*i + m*j + l*k;
            ans = min(ans, v);
        }
    }
    cout << ans << endl;
}
