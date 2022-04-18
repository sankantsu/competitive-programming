// E - Colorful Hats 2
// https://atcoder.jp/contests/sumitrust2019/tasks/sumitb2019_e
#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint1000000007;

int n;
int a[100000];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    vector<int> cnt(3);
    mint res = 1;
    rep(i,n) {
        int x = 0;
        rep(j,3) {
            if (a[i] == cnt[j]) {
                if (x == 0) cnt[j]++;
                x++;
            }
        }
        res *= x;
    }
    cout << res.val() << endl;
}
