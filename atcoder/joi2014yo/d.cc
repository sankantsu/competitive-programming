// D - 部活のスケジュール表
// https://atcoder.jp/contests/joi2014yo/tasks/joi2014yo_d
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 1000;
const int mod = 10007;

int n;
string s;

int dp[max_n+1][8];

int chief(int i) {
    char c = s[i];
    int chief;
    switch(c){
        case('J'):
            chief = 1; break;
        case('O'):
            chief = 2; break;
        case('I'):
            chief = 4; break;
    }
    return chief;
}

int main() {
    cin >> n;
    cin >> s;
    dp[0][1] = 1;
    rep(i,n) {
        int c = chief(i);
        rep(j,8) {
            if ((j&c) == 0) continue;
            rep(k,8) {
                if (j&k) {
                    dp[i+1][j] += dp[i][k];
                }
            }
            dp[i+1][j] = dp[i+1][j]%mod;
        }
    }
    int sum = 0;
    rep(j,8) sum = (sum+dp[n][j])%mod;
    cout << sum << endl;
}
