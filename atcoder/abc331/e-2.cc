#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, m, l;
    cin >> n >> m >> l;

    vector<long> a(n), b(m);
    rep(i,n) cin >> a[i];
    rep(i,m) cin >> b[i];

    using P = pair<size_t, size_t>;
    set<P> ng;
    rep(i,l) {
        size_t c, d;
        cin >> c >> d;
        c--; d--;
        ng.emplace(c, d);
    }

    vector<size_t> idx(m);
    rep(i,m) idx[i] = i;
    sort(idx.begin(), idx.end(), [&](size_t i, size_t j) { return b[i] > b[j]; });

    long ans = 0;
    rep(i,n) {
        rep(j,m) {
            auto p = make_pair(i, idx[j]);
            if (!ng.count(p)) {
                ans = max(ans, a[i] + b[idx[j]]);
                break;
            }
        }
    }
    cout << ans << endl;
}
