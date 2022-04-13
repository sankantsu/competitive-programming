// AC O(NK 2^K) K=26

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint998244353;

constexpr int mod = 998244353;
constexpr int charsetsize = 26;

int n, l;
string ss[18];

mint fact[charsetsize+1];
mint inv[charsetsize+1];
mint fact_inv[charsetsize+1];

mint dp[charsetsize+1];
bool set[1<<26];

// O(K)
void init_binomial() {
    fact[0] = 1; fact[1] = 1;
    fact_inv[0] = 1; fact_inv [1] = 1;
    inv[1] = 1;
    for (int i = 2; i < charsetsize+1; i++) {
        fact[i] = i*fact[i-1];
        inv[i] = (-1)*inv[mod%i]*(mod/i);
        fact_inv[i] = inv[i]*fact_inv[i-1];
    }
}

// O(1)
mint binomial(int n, int k) {
    return fact[n]*fact_inv[k]*fact_inv[n-k];
}

// O(K^2 logL)
void calc_fixed_length() {
    init_binomial();
    dp[1] = 1;
    for (int i = 2; i <= charsetsize; i++) {
        dp[i] = mint(i).pow(l);
        for (int j = 1; j < i; j++) {
            dp[i] -= binomial(i,j)*dp[j];
        }
    }
}

// O(NK 2^K)
void calc_set() {
    for (int i = 0; i < n; i++) {
        int s = 0;
        for (auto c : ss[i]) {
            int k = c - 'a';
            s |= (1<<k);
        }
        /* cout << "i,s: " << i << " " << s << endl; */
        int sub = s;
        do {
            set[sub] = true;
            sub = (sub-1)&s;
        } while (sub != 0);
    }
}

// O(log x)
int pop_count(int x) {
    int cnt = 0;
    while (x > 0) {
        if (x&1) cnt++;
        x >>= 1;
    }
    return cnt;
}

// O(2^K)
mint add_all() {
    mint res = 0;
    for (int i = 1; i < (1<<charsetsize); i++) {
        if (set[i]) {
            /* cout << "i: " << i << endl; */
            res += dp[pop_count(i)];
        }
    }
    return res;
}

int main() {
    cin >> n >> l;
    for (int i = 0; i < n; i++) cin >> ss[i];

    calc_fixed_length();
    calc_set();
    mint res = add_all();
    cout << res.val() << endl;
}
