// 多重ループ https://atcoder.jp/contests/abc021/tasks/abc021_d
#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint1000000007;
constexpr long mod = 1000000007;

int n,k;

constexpr int max_n = 200000;
mint fact[max_n];
mint inv[max_n];
mint fact_inv[max_n];

void init() {
    fact[0] = fact[1] = 1;
    inv[1] = 1;
    fact_inv[0] = fact_inv[1] = 1;
    for (int i = 2; i < max_n; i++) {
        fact[i] = i*fact[i-1];
        inv[i] = -(mod/i)*inv[mod%i];
        fact_inv[i] = inv[i]*fact_inv[i-1];
    }
}

mint binomial(int n, int k) {
    return fact[n]*fact_inv[k]*fact_inv[n-k];
}

int main() {
    cin >> n >> k;
    init();
    cout << binomial(n+k-1,k).val() << endl;
}
