#include <bits/stdc++.h>
#include <atcoder/all>

using namespace std;
using namespace atcoder;
using mint = modint998244353;
// using mint = modint1000000007;

int n;
int a[300000];
// int sum[300000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];

    /* sum[0] = a[0]; */
    /* for (int i = 1; i < n; i++) sum[i] = sum[i-1]+a[i]; */

    int last = a[0];
    int cur = 1;
    int len[2] = {0,0};
    len[last] = 1;
    for (int i = 1; i < n; i++) {
        if (last == a[i]) {
            cur++;
        }
        else {
            last = a[i];
            cur = 1;
        }
        if (cur > len[last]) {
            len[last] = cur;
        }
    }
    cout << (1 + len[0] + len[1]) << endl;
}
