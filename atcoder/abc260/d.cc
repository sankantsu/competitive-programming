#include <iostream>
#include <vector>
#include <map>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> p(n);
    rep(i,n) {
        cin >> p[i];
        p[i]--;
    }

    map<long, vector<long>> mp;
    vector<long> ans(n, -2);
    rep(i,n) {
        auto it = mp.lower_bound(p[i]);
        if (it == mp.end()) {
            vector<long> v {p[i]};
            auto [it2, ok] = mp.emplace(p[i], std::move(v));
            assert(ok);
            it = it2;
        }
        else {
            vector<long> v(std::move(it->second));
            v.push_back(p[i]);
            mp.erase(it);
            auto [it2, ok] = mp.emplace(p[i], std::move(v));
            assert(ok);
            it = it2;
        }
        if (it->second.size() == k) {
            for (auto x : it->second) {
                ans[x] = i;
            }
            mp.erase(it);
        }
    }
    rep(i,n) cout << ans[i] + 1 << endl;
}
