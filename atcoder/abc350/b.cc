#include <iostream>
#include <vector>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, q;
    cin >> n >> q;

    vector<long> t(q);
    rep(i,q) {
        cin >> t[i];
        t[i]--;
    }

    vector<int> a(n,1);
    rep(i,q) {
        a[t[i]] = 1 - a[t[i]];
    }

    long ans = 0;
    rep(i,n) ans += a[i];

    cout << ans << endl;
}
