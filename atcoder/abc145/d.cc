#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

constexpr long mod = 1000000007;
int x,y;

mint fact[1000001];
mint inv[1000001];
mint fact_inv[1000001];

void init() {
    fact[0] = 1;
    fact[1] = 1;
    inv[1] = 1;
    fact_inv[0] = 1;
    fact_inv[1] = 1;
    for (int i = 2; i <= 1000000; i++) {
        fact[i] = i*fact[i-1];
        inv[i] = -(mod/i)*inv[mod%i];
        fact_inv[i] = inv[i]*fact_inv[i-1];
    }
}

mint binomial(int n, int k) {
    return fact[n]*fact_inv[k]*fact_inv[n-k];
}

int main() {
    cin >> x >> y;
    if ((x+y)%3 != 0) {
        cout << 0 << endl;
        return 0;
    }
    int k = (x+y)/3;
    x -= k;
    y -= k;
    if (x < 0 || y < 0) {
        cout << 0 << endl;
        return 0;
    }
    init();
    /* cout << binomial(3,0).val() << endl; */
    mint res = binomial(x+y,x);
    cout << res.val() << endl;
}
