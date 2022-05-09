#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

unsigned int hash_int(unsigned int x) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

template <typename Vec>
auto make_prefix_hash(const Vec &v) {
    using T = typename Vec::value_type;
    set<T> s;
    unsigned int h = 0;
    Vec vh;
    for (auto x : v) {
        if (s.find(x) == s.end()) {
            s.insert(x);
            h ^= hash_int(x);
        }
        vh.push_back(h);
    }
    return vh;
}

int main() {
    int n;
    cin >> n;
    vector<int> a(n);
    vector<int> b(n);
    rep(i,n) cin >> a[i];
    rep(i,n) cin >> b[i];
    int q;
    cin >> q;
    vector<int> x(q);
    vector<int> y(q);
    rep(i,q) {
        cin >> x[i] >> y[i];
        x[i]--; y[i]--;
    }

    auto hash_a = make_prefix_hash(a);
    auto hash_b = make_prefix_hash(b);
    rep(i,q) {
        if (hash_a[x[i]] == hash_b[y[i]]) {
            cout << "Yes" << endl;
        }
        else {
            cout << "No" << endl;
        }
    }
}
