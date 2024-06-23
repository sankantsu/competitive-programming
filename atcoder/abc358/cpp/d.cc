#include <iostream>
#include <vector>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    map<long, size_t> a;
    rep(i,n) {
        long x;
        cin >> x;
        a[x]++;
    }

    vector<long> b(m);
    rep(i,m) cin >> b[i];

    long ans = 0;
    rep(i,m) {
        auto it = a.lower_bound(b[i]);
        if (it == a.end()) {
            ans = -1;
            break;
        }
        auto [x, cnt] = *it;
        ans += x;
        a[x]--;
        if (a[x] == 0) {
            a.erase(it);
        }
    }
    cout << ans << endl;
}
