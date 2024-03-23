#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> h(n);
    rep(i,n) {
        cin >> h[i];
    }
    sort(h.begin(), h.end());

    long ans = 1L<<60;
    rep(i, n-k+1) {
        ans = min(ans, h[i + k - 1] - h[i]);
    }
    cout << ans << endl;
}
