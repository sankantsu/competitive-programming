#include <iostream>
#include <vector>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// e.g. l = 3, r = 19
// l = 00011
// r = 10011
// k = 10000

int main() {
    long l, r;
    cin >> l >> r;

    long k;
    long m = 0;
    for (int i = 60; i >= 0; i--) {
        long mask = 1L<<i;
        if (r&mask) {
            m |= mask;
        }
        if ((l&mask) != (r&mask)) {
            k = i;
            break;
        }
    }
    cerr << "m: " << m << endl;

    using P = pair<long, long>;
    vector<P> ans;
    while (l < m) {
        long x = m + l;
        long lsb = x&-x;
        ans.emplace_back(l, l+lsb);
        l += lsb;
    }

    for (int j = k-1; j >= 0; j--) {
        long mask = 1L<<j;
        if (r&mask) {
            ans.emplace_back(l, l+mask);
            l += mask;
        }
    }

    cout << ans.size() << endl;
    for (auto [x,y] : ans) {
        cout << x << " " << y << endl;
    }
}
