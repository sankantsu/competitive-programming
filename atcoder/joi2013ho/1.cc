// 1 - 電飾
// https://atcoder.jp/contests/joi2013ho/tasks/joi2013ho1
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int a[100000];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    int c = a[0];
    int l1 = 0;
    int l2 = 0;
    int l3 = 1;
    int res = 1;
    /* cout << c << " "; */
    rep(i,n-1) {
        int cur = a[i+1];
        if (i%2 == 0) cur ^= 1;
        /* cout << cur << " "; */
        if (cur == c) {
            l3++;
        }
        else {
            c = cur;
            l1 = l2;
            l2 = l3;
            l3 = 1;
        }
        /* cout << i+1 << " " << l1 << " " << l2 << " " << l3 << endl; */
        res = max(res,l1+l2+l3);
    }
    /* cout << endl; */
    cout << res << endl;
}
