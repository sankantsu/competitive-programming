#include <bits/stdc++.h>
#include <atcoder/all>

using namespace std;
using namespace atcoder;
using mint = modint998244353;
// using mint = modint1000000007;

int n;
int a[300000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];

    int cur = 0; // 区間の1の個数 - 0の個数
    int low = 0;
    int high = 0;
    int max_point = 0;
    int min_point = 0;
    for (int i = 0; i < n; i++) {
        if (a[i] == 1) {
            cur++;
            if (cur > high) {
                high = cur;
            }
            if (min_point < cur - low) {
                min_point = cur - low;
            }
        }
        else {
            cur--;
            if (cur < low) {
                low = cur;
                max_point = high - low;
            }
            if (max_point < high - cur) {
                max_point = high - cur;
            }
        }
    }
    /* cout << "min: " << -min_point << ", max: " << max_point << endl; */
    cout << (1 + max_point + min_point) << endl;
}
