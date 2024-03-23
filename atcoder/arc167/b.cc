#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <atcoder/modint.hpp>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

auto factoize(long n) {
    using P = pair<long, mint>;
    vector<P> res;
    for (long p = 2; p*p <= n; p++) {
        size_t cnt = 0;
        while (n%p == 0) {
            cnt++;
            n /= p;
        }
        if (cnt > 0) {
            res.emplace_back(p, cnt);
        }
    }
    if (n != 1) res.emplace_back(n, 1);  // n is prime
    return res;
}

bool is_square(long n) {
    long m = 2;
    while (m*m < n) m++;
    if (m*m == n) {
        return true;
    }
    else {
        return false;
    }
}

int main() {
    long a, b;
    cin >> a >> b;

    bool sq = is_square(a);
    auto fs = factoize(a);
    mint ndiv = 1;
    for (auto [p, cnt] : fs) {
        ndiv *= mint(b)*cnt + 1;
    }

    mint ans = 0;
    if (sq && b%2 == 1) {
        // square number
        ans += b/2;
        ans += ((ndiv - 1)/2)*b;
    }
    else {
        ans += (ndiv/2)*b;
    }
    cout << ans.val() << endl;
}
