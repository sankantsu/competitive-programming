#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

constexpr int mod = 1000000007;

int w,h;

mint fact[200000];
mint inv[200000];
mint fact_inv[200000];

void init() {
    fact[0] = 1;
    fact[1] = 1;
    inv[1] = 1;
    fact_inv[1] = 1;
    for (int i = 2; i <= w+h; i++) {
        fact[i] = i*fact[i-1];
        inv[i] = -(mod/i)*inv[mod%i];
        fact_inv[i] = inv[i]*fact_inv[i-1];
        /* cout << i << " " << fact[i].val() << " " << inv[i].val() << " " << fact_inv[i].val() << endl; */
    }
}

mint binomial(int n, int k) {
    return fact[n]*fact_inv[k]*fact_inv[n-k];
}

int main() {
    cin >> w >> h;
    w--; h--;
    init();
    cout << binomial(w+h,w).val() << endl;
}
