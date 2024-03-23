#include <iostream>
#include <vector>
#include <atcoder/modint.hpp>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;

// E(N,K): N 種のうち K 種すでに出ている状態でターンが回ってきたときの罰金の期待値
// 1人目: E(N, 0)
// 2人目: E(N, 1)
// E(N,N) = 0
// E(N,N-1) = (N-1)/N (1 + (N-1)/N E(N,N-1)) = N-1/N + (N-1)^2/N^2 E(N,N-1)
// E(N,K) = K/N (1 + (K/N E(N,K) + (N-K)/N E(N,K+1))) + (N-K)/N ((K+1)/N E(N,K+1) + (N-K-1)/N E(N,K+2))
//        = K/N + (K/N)^2 E(N,K) + (K(N-K) + (N-K)(K+1))/N^2 E(N,K+1) + (N-K)(N-K-1)/N^2 E(N,K+2)
//        = K/N + K^2/N^2 E(N,K) + (N-K)(2K + 1)/N^2 E(N,K+1) + (N-K)(N-K-1)/N^2 E(N,K+2)  (K <= N-2)
//
// (1 - (N-1)^2/N^2) E(N,N-1) = N-1/N
// (N^2 - (N^2 - 2N + 1)/N^2) E(N,N-1) = N-1/N
// (2N - 1)/N^2 E(N,N-1) = N-1/N
// E(N,N-1) = N(N-1)/(2N - 1)
//
// (N^2 - K^2)/N^2 E(N,K) = K/N + (N-K)(2K + 1)/N^2 E(N,K+1) + (N-K)(N-K-1)/N^2 E(N,K+2)
// E(N,K) = (NK + (N-K)(2K + 1)E(N,K+1) + (N-K)(N-K-1)E(N,K+2))/(N^2 - K^2) 

int main() {
    long N;
    cin >> N;

    mint n = N;
    vector<mint> dp(N+1);  // dp[i] = E(N,i);
    dp[N] = 0;
    dp[N-1] = n*(n-1)/(2*n - 1);
    for (long i = N-2; i >= 0; i--) {
        dp[i] = (n*i + (n-i)*(2*i+1)*dp[i+1] + (n-i)*(n-i-1)*dp[i+2])/(n*n - i*i);
    }
    cout << dp[0].val() << endl;
    cout << dp[1].val() << endl;
}
