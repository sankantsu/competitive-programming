#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <set>
#include <atcoder/modint.hpp>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

using mint = atcoder::modint998244353;

int main() {
    long n;
    cin >> n;

    vector<long> a(n + 1);
    for (long i = 1; i <= n; i++) cin >> a[i];

    vector<long> last(n + 1);
    for (long i = 1; i <= n; i++) last[i] = i;
    vector<bool> used(n + 1);

    bool check = true;
    rep(i,n) {
        long j = n - i;
        if (a[j] < j) {
            check = false;
            break;
        }
        if (a[j] > j) {
            if (a[a[j]] != a[j]) {
                check = false;
                break;
            }
            used[last[a[j]]] = true;
            last[a[j]] = j;
        }
    }
    if (!check) {
        cout << 0 << endl;
        return 0;
    }

    mint ans = 1;
    long cnt = 0;
    for (long i = 1; i <= n; i++) {
        if (used[i]) {
            cnt++;
        }
        if (a[i] == i) {
            long k = i - cnt;
            ans *= k;
            cnt++;
        }
    }
    cout << ans.val() << endl;
}
