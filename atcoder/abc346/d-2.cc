#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    string s;
    cin >> s;

    vector<long> c(n);
    rep(i,n) cin >> c[i];

    vector<long> v;
    rep(i,n) v.push_back(s[i] - '0');
    rep(i,n) if (i % 2 == 0) v[i] = 1 - v[i];

    const long inf = 1L<<60;

    long ans = inf;
    rep(_,2) {
        vector<long> l(n+1);
        vector<long> r(n+1);
        rep(i,n) {
            l[i+1] = l[i];
            if (v[i] == 1) l[i+1] += c[i];
        }
        for (long i = n - 1; i >= 0; i--) {
            r[i] = r[i+1];
            if (v[i] == 0) r[i] += c[i];
        }
        for (long i = 1; i <= n-1; i++) ans = min(ans, l[i] + r[i]);

        // reverse numbers
        rep(i,n) v[i] = 1 - v[i];
    }

    cout << ans << endl;
}
