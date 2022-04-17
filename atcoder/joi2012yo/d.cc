// D - パスタ
// https://atcoder.jp/contests/joi2012yo/tasks/joi2012yo_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr int mod = 10000;

int n,k;
int a[110];
int b[110];

int menu[110];
int dp[110][3][3]; // i, prev, cur


int main() {
    cin >> n >> k;
    rep(i,k) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }
    rep(i,n) {
        menu[i] = -1;
    }
    rep(i,k) {
        menu[a[i]] = b[i];
    }
    dp[0][0][0] = 1;
    rep(i,n) {
        rep(k,3) {
            if (menu[i] != -1 && k != menu[i]) continue;
            rep(j,3) {
                int sum = 0;
                rep(l,3) {
                    if (i >= 2 && j == k && k == l) continue;
                    sum = (sum+dp[i][l][j])%mod;
                }
                dp[i+1][j][k] = sum;
            }
        }
    }
    int sum = 0;
    rep(j,3) {
        rep(k,3) {
            sum = (sum+dp[n][j][k])%mod;
        }
    }
    cout << sum << endl;
}
