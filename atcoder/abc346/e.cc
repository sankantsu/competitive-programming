#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <map>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long h, w, m;
    cin >> h >> w >> m;

    vector<long> T(m);
    vector<long> A(m);
    vector<long> X(m);
    rep(i,m) {
        long t, a, x;
        cin >> t >> a >> x;
        a--;
        T[i] = t;
        A[i] = a;
        X[i] = x;
    }

    map<long, size_t> cnt;
    set<long> rows, cols;
    for (long i = m-1; i >= 0; i--) {
        long t = T[i];
        long a = A[i];
        long x = X[i];
        if (t == 1) {
            if (rows.find(a) != rows.end()) continue;
            rows.insert(a);
            long c = w - cols.size();
            if (c > 0) cnt[x] += c;
        }
        else {
            if (cols.find(a) != cols.end()) continue;
            cols.insert(a);
            long c = h - rows.size();
            if (c > 0) cnt[x] += c;
        }
    }

    long s = 0;
    for (auto [x, c] : cnt) {
        if (x != 0) s += c;
    }

    long c0 = h*w - s;
    if (c0 > 0) {
        cnt[0] = c0;
    }

    cout << cnt.size() << endl;
    for (auto [x, c] : cnt) {
        cout << x << " " << c << endl;
    }
}
