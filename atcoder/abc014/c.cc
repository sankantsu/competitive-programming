// C - AtColor
// https://atcoder.jp/contests/abc014/tasks/abc014_3
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int a[200000];
int b[200000];

constexpr int k = 1000000;
int cnt[k+1];
int sum[k+2];

int main() {
    cin >> n;
    rep(i,n) {
        cin >> a[i] >> b[i];
    }
    rep(i,n) {
        cnt[a[i]]++;
        cnt[b[i]+1]--;
    }
    rep(i,k+1) {
        sum[i+1] = sum[i]+cnt[i];
    }
    int mx = -1;
    rep(i,k+2) {
        mx = max(mx,sum[i]);
    }
    cout << mx << endl;
}
