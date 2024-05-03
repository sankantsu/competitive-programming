#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <atcoder/modint.hpp>
#include <set>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    vector<int> a(m), b(m), c(m);
    rep(i,m) {
        cin >> a[i] >> b[i] >> c[i];
        a[i]--; b[i]--; c[i]--;
    }

    vector<int> mx(n, n-1);
    rep(i,m) {
        mx[a[i]] = min(mx[a[i]], c[i]);
        mx[b[i]] = min(mx[b[i]], c[i]);
    }

    set<int> idx;
    rep(i,n) idx.insert(i);
    rep(i,m) {
        idx.erase(a[i]);
        idx.erase(b[i]);
    }

    set<int> st;
    rep(i,n) st.insert(i);
    rep(i,m) st.erase(c[i]);
    if (!st.empty()) {
        int x = *st.rbegin();
        for (auto i : idx) mx[i] = x;
    }

    vector<int> cnt(n);
    rep(i,n) {
        cnt[mx[i]]++;
    }

    mint ans = 1;
    int s = 0;
    for (int v = n - 1; v >= 0; v--) {
        s += cnt[v];
        /* cerr << "v, s: " << v << " " << s << endl; */
        ans *= s;
        s--;
    }
    cout << ans.val() << endl;
}
