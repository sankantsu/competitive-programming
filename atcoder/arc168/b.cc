#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    sort(a.begin(), a.end());

    long g = 0;
    rep(i,n) {
        g ^= a[i];
    }
    
    if (g != 0) {
        cout << -1 << endl;  // arbitrary large k is ok
        return 0;
    }

    long ans = 0;
    rep(i,n) {
        long j = n - 1 - i;
        auto ub = upper_bound(a.begin(), a.end(), a[j]);
        auto lb = lower_bound(a.begin(), a.end(), a[j]);
        long cnt = distance(lb, ub);

        if (cnt%2 == 0) continue;
        else {
            ans = a[j] - 1;
            break;
        }
    }
    cout << ans << endl;
}
