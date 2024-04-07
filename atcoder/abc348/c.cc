#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n), c(n);
    rep(i,n) cin >> a[i] >> c[i];

    const long inf = 1L<<60;
    map<long, long> mp;  // color -> min a
    rep(i,n) {
        if (mp.find(c[i]) == mp.end()) {
            mp[c[i]] = inf;
        }
        mp[c[i]] = min(mp[c[i]], a[i]);
    }

    long ans = -1;
    for (auto [c, a] : mp) {
        ans = max(ans, mp[c]);
    }
    cout << ans << endl;
}
