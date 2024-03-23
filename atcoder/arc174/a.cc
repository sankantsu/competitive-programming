#include <iostream>
#include <vector>
#include <random>
#include <tuple>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, c;
    cin >> n >> c;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    if (c <= 0) rep(i,n) a[i] = -a[i];

    long s = 0, m = 0;
    long mx = -(1L<<60);
    rep(i,n) {
        s = s + a[i];
        m = min(m, s);
        mx = max(mx, s - m);
    }

    long ans = s + (c - 1)*mx;
    if (c <= 0) ans = -ans;

    cout << ans << endl;
}
