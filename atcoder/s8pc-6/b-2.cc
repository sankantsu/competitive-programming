// AtCoder Market
// https://atcoder.jp/contests/s8pc-6/tasks/s8pc_6_b

// O(N logN)
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
long a[50];
long b[50];

int main() {
    cin >> n;
    rep(i,n) {
        cin >> a[i] >> b[i];
        a[i]--; b[i]--;
    }
    sort(a,a+n);
    sort(b,b+n);
    long ent = a[n/2];
    long ext = b[n/2];
    long res = 0;
    rep(i,n) {
        res += b[i] - a[i];
        res += abs(ent-a[i]);
        res += abs(ext-b[i]);
    }
    cout << res << endl;
}
