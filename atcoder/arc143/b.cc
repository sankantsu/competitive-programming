#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

constexpr int mod = 998244353;

constexpr int max_n = 501;
constexpr int max_m = max_n*max_n;

struct Binomial {
    Binomial();
    mint calc_fact(int k) {
        return fact[k];
    }
    mint calc_binomial(int k, int r) {
        return fact[k]*fact_inv[r]*fact_inv[k-r];
    }
    private:
    mint fact[max_m];
    mint inv[max_m];
    mint fact_inv[max_m];
};

Binomial::Binomial() {
    fact[0] = 1;
    fact[1] = 1;
    inv[1] = 1;
    fact_inv[1] = 1;
    for (int i = 2; i < max_m; i++) {
        fact[i] = i*fact[i-1];
        inv[i] = -(mod/i)*inv[mod%i];
        fact_inv[i] = inv[i]*fact_inv[i-1];
    }
}

int main() {
    int n;
    cin >> n;

    if (n == 1) {
        cout << 0 << endl;
    }
    else {
        Binomial bi;
        mint all = bi.calc_fact(n*n);
        mint ng = n*n * bi.calc_binomial(n*n,2*n-1) * bi.calc_fact(n-1)*bi.calc_fact(n-1) * bi.calc_fact((n-1)*(n-1));
        mint res = all - ng;
        cout << res.val() << endl;
    }
}
