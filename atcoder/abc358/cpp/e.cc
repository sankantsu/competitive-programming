#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

// i 番目の文字まで使ってできるちょうど j 文字の文字列の数を dp[i][j] とする。
// i+1 番目まで考えるとき、i+1 番目の文字を k 個使ってできる j 文字の文字列の数を f(k) とすると
// f(i,k) = dp[i][j - k] * Combination(j, k)
// dp[i+1][j] = Sum_k f(i, k)
//
// 計算量
// - アルファベットの数 S = 26, 
// - 文字列の長さの最大値 K <= 1000
// f(i,k): O(1) (前処理 O(K))
// dp 遷移 1回あたり: k*O(1) (k <= K)
// dp テーブルサイズ: S * K

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a#5-%E4%BA%8C%E9%A0%85%E4%BF%82%E6%95%B0-ncr

long MAX = 2000;
long MOD = 998244353;

long fac[2000];
long finv[2000];
long inv[2000];

// テーブルを作る前処理
void COMinit() {
    fac[0] = fac[1] = 1;
    finv[0] = finv[1] = 1;
    inv[1] = 1;
    for (int i = 2; i < MAX; i++){
        fac[i] = fac[i - 1] * i % MOD;
        inv[i] = MOD - inv[MOD%i] * (MOD / i) % MOD;
        finv[i] = finv[i - 1] * inv[i] % MOD;
    }
}

// 二項係数計算
long long COM(int n, int k){
    if (n < k) return 0;
    if (n < 0 || k < 0) return 0;
    return fac[n] * (finv[k] * finv[n - k] % MOD) % MOD;
}

long dp[2000][2000];

int main() {
    COMinit();

    int k;
    cin >> k;

    vector<long> c(26);
    rep(i, 26) cin >> c[i];

    dp[0][0] = 1;
    rep(i,26) {
        rep(j,k+1) {
            rep(l, min(1+c[i], k + 1 - j)) {
                dp[i+1][j+l] = (dp[i+1][j+l] + dp[i][j] * COM(j + l, l)) % MOD;
            }
        }
    }
    long ans = 0;
    rep(j, k+1) {
        ans = (ans + dp[26][j]) % MOD;
    }
    cout << ans - 1 << endl;
}
