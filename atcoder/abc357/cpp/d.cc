#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <atcoder/modint.hpp>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

// n のケタ数を m とする。
// V_n = n * (1 + 10^m + 10^(2*m) + ... + 10^((n - 1) * m))
// V_n mod p = (n mod p) * ((1 mod p) + (10^m mod p) + ... (10^((n - 1) * m) mod p)) mod p
//
// r = 10^m とすると、
//     V_n = n * (1 + r + ... + r^(n - 1))
// r * V_n = n * (    r + ... + r^(n - 1) + r^n)
//
// (r - 1) * V_n = n * (r^n - 1)
// V_n = n * (r^n - 1) / (r - 1)

using namespace std;

int ndigit(long n) {
    int res = 1;
    while (n >= 10) {
        n /= 10;
        res++;
    }
    return res;
}

int main() {
    long n;
    cin >> n;

    int m = ndigit(n);

    mint r = mint(10).pow(m);
    mint ans = mint(n) * (r.pow(n) - 1) / mint(r - 1);
    cout << ans.val() << endl;
}
