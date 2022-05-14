// D - ぬいぐるみの整理
// https://atcoder.jp/contests/joi2017yo/tasks/joi2017yo_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 100000;
const long max_m = 20;

long n,m;
long a[max_n+10];

long sum[max_m+10][max_n+10];
long freq[max_m+10];
long dp[1<<21];

void count_frequency() {
    rep(i,n) {
        freq[a[i]]++;
        rep(k,m) {
            sum[k][i+1] = sum[k][i];
            if (k == a[i]) sum[k][i+1] += 1;
        }
    }
}

long count_diff(int t, int k) {
    long j = 0;
    rep(k_,m) {
        if ((t>>k_)&1) {
            j += freq[k_];
        }
    }
    long r = sum[k][j+freq[k]];
    long l = sum[k][j];
    long res = freq[k]-(r-l);
    return res;
}

long rec(int s) {
    if (dp[s] != -1) {
        return dp[s];
    }
    if (s == 0) {
        dp[s] = 0;
        return dp[s];
    }
    const long inf = 2*n;
    long res = inf;
    rep(k,m) {
        if ((s>>k)&1) {
            int t = s&~(1<<k);
            res = min(res,rec(t)+count_diff(t,k));
        }
    }
    dp[s] = res;
    return res;
}

void input() {
    cin >> n >> m;
    rep(i,n) {
        cin >> a[i];
        a[i]--;
    }
}

int main() {
    input();
    count_frequency();

    rep(s,1<<m) dp[s] = -1;
    long res = rec((1<<m)-1);
    cout << res << endl;
}
