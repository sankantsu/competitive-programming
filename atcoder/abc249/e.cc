#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n,p;

long dp[3100][3100]; // |S| = i, |T| = j を満たす S の数
long sum[3100][3100]; // sum[i+1][j] = sum[0][j] + ... + sum[i][j];

// digit(k): k の桁数
// |S|=i なる S の末尾に長さ k の文字列をつけたものを S' とすると，|S'|=i+k, |T'|=j+1+digit(k)
// dp[i+k][j+1+digit(k)] += 25*dp[i][j]
// -> dp[i][j] = sum_{k=1->i} dp[i-k][j-1-digit(k)]
//             = sum_{K=1->4} sum_{k=10^(K-1)->10^K-1} dp[i-k][j-1-K]
//             = sum_{K=1->4} sum[i-10^(K-1)+1][j-1-K] - sum[i-(10^K-1)][j-1-K]

/* long pow_mod(long a, long b, long p) { */
/*     if (b <= 0) return 1; */
/*     return a*pow_mod(a,b-1,p)%p; */
/* } */

constexpr long ten_pow[] = {1L,10L,100L,1000L,10000L};

int main() {
    cin >> n >> p;
    dp[0][0] = 1;
    rep(i,n+1) rep(j,n+1) {
        for (int k = 1; k <=4; k++) {
            if (j-1-k < 0) break;
            /* long x = max(0LL,i-pow_mod(10,k-1,p)+1); */
            /* long y = max(0LL,i-pow_mod(10,k,p)+1); */
            long x = max(0LL,i-ten_pow[k-1]+1);
            long y = max(0LL,i-ten_pow[k]+1);
            long diff = sum[x][j-1-k] - sum[y][j-1-k] + p;
            diff = (j-1-k == 0) ? 26*diff : 25*diff;
            dp[i][j] = (dp[i][j]+diff)%p;
        }
        sum[i+1][j] = (sum[i][j]+dp[i][j])%p;
    }
    long res = 0;
    rep(j,n) {
        res = (res+dp[n][j])%p;
    }
    cout << res << endl;
}
