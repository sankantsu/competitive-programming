#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr int max_digits = 16;
constexpr int max_k = 9*14+1;

long dp[2][max_digits][max_k][max_k];  // exact, digits, mod, digits sum

long solve_k(const string& n, long k) {
    fill((long*)dp, (long*)dp+2*max_digits*max_k*max_k, 0);
    dp[1][0][0][0] = 1;
    rep(i, max_digits - 1) {
        long d = n[i+1] - '0';
        rep(mod, k) {
            rep(s, max_k) {
                rep(j, 10) {  // i+1 th digit
                    long x = (mod*10 + j) % k;
                    long y = s + j;
                    if (j == d) {
                        dp[1][i+1][x][y] += dp[1][i][mod][s];
                    }
                    dp[0][i+1][x][y] += dp[0][i][mod][s];
                    if (j < d) {
                        dp[0][i+1][x][y] += dp[1][i][mod][s];
                    }
                }
            }
        }
    }
    return dp[1][max_digits-1][0][k] + dp[0][max_digits-1][0][k];
}

int main() {
    string n;
    cin >> n;

    string pad(max_digits - n.size(), '0');  // zero padding
    n = pad + n;

    long ans = 0;
    for (long k = 1; k < max_k; k++) {
        long ans_k = solve_k(n, k);
        /* if (k <= 10) { */
        /*     cerr << "k, ans_k: " << k << " " << ans_k << endl; */
        /* } */
        ans += ans_k;
    }
    cout << ans << endl;
}
