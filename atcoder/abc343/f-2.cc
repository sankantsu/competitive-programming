#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <map>
#include <atcoder/segtree>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using namespace atcoder;

struct S {
    long max;
    size_t n_max;
    long second;
    size_t n_second;
};

S e() {
    return S{-1, 0, -1, 0};
}

S op(S a, S b) {
    S res = e();
    map<long, size_t> mp;
    mp[a.max] += a.n_max;
    mp[a.second] += a.n_second;
    mp[b.max] += b.n_max;
    mp[b.second] += b.n_second;
    long j = 0;
    auto it = mp.rbegin();
    res.max = it->first; res.n_max = it->second;
    it++;
    if (it != mp.rend()) {
        res.second = it->first; res.n_second = it->second;
    }
    return res;
}

int main() {
    long n, q;
    cin >> n >> q;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    vector<S> s;
    rep(i,n) {
        s.push_back(S{a[i], 1, -1, 0});
    }
    segtree<S, op, e> seg(s);
    rep(i,q) {
        /* rep(i,n) { */
        /*     S s = seg.get(i); */
        /*     cerr << s.max << "," << s.n_max << "," << s.second << "," << s.n_second << " "; */
        /* } */
        /* cerr << endl; */
        long type;
        cin >> type;
        if (type == 1) {
            long p, x;
            cin >> p >> x;
            p--;
            seg.set(p, S{x, 1, -1, 0});
        }
        if (type == 2) {
            long l, r;
            cin >> l >> r;
            l--;
            S res = seg.prod(l,r);
            cout << res.n_second << endl;
        }
    }
}
