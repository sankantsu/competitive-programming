#include <iostream>
#include <vector>
#include <atcoder/modint.hpp>
#include <atcoder/lazysegtree.hpp>

using mint = atcoder::modint998244353;
using atcoder::lazy_segtree;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

struct S {
    mint a;
    mint b;
};

S e() {
    return S{1, 0};
}

// c(ax + b) + d
// = acx + (bc + d)
S op(S lhs, S rhs) {
    mint a = rhs.a;
    mint b = rhs.b;
    mint c = lhs.a;
    mint d = lhs.b;
    return S{a*c, b*c + d};
}

using segtree = lazy_segtree<S, op, e, S, op, op, e>;

int main() {
    long n, m;
    cin >> n >> m;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    segtree seg(n);
    rep(_,m) {
        long l, r, y;
        cin >> l >> r >> y;
        l--;

        // (r - l - 1)/(r - l) x + 1/(r - l) y
        mint a = mint(r - l - 1)/(r - l);
        mint b = mint(y)/(r - l);
        seg.apply(l, r, S{a, b});
    }

    vector<mint> ans(n);
    rep(i,n) {
        auto [c, d] = seg.get(i);
        ans[i] = c*a[i] + d;
    }

    rep(i,n) cout << ans[i].val() << " "; cout << endl;
}
