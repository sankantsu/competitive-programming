#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 100000;
const int mod = 1000000007;

int n;

long fact[max_n+10];
long inv[max_n+10];
long fact_inv[max_n+10];

void init() {
    fact[0] = 1; fact[1] = 1;
    fact_inv[0] = 1; fact_inv[1] = 1;
    inv[1] = 1;
    for (int i = 2; i <= max_n; i++) {
        fact[i] = (i*fact[i-1])%mod;
        inv[i] = mod - (mod/i)*inv[mod%i]%mod;
        fact_inv[i] = (inv[i]*fact_inv[i-1])%mod;
    }
}

long mod_binomial(int n, int k) {
    long res;
    res = (fact[n]*fact_inv[k])%mod;
    res = (res*fact_inv[n-k])%mod;
    return res;
}

long solve(int k) {
    long max_a = 1 + (n-1)/k;
    long res = 0;
    for (int a = 1; a <= max_a; a++) {
        res = (res + mod_binomial(n-(k-1)*(a-1),a))%mod;
    }
    return res;
}

int main() {
    init();
    cin >> n;

    for (int k = 1; k <= n; k++) {
        long res = solve(k);
        cout << res << endl;
    }
}
