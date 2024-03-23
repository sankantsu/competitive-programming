#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <atcoder/modint.hpp>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using namespace atcoder;

using mint = modint998244353;

int main() {
    long n, k, l;
    cin >> n >> k >> l;

    if (l < n - k + 1) {
        cout << 0 << endl;
        return 0;
    }

    mint ans = 1;
    rep(i, n - k) {
        ans *= l - i;
    }
    rep(i, k) {
        ans *= l - (n - k);
    }
    cout << ans.val() << endl;
}
