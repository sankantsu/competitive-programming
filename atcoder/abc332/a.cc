#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, s, k;
    cin >> n >> s >> k;

    vector<long> p(n), q(n);
    rep(i,n) cin >> p[i] >> q[i];

    long ans = 0;
    rep(i,n) ans += p[i]*q[i];
    if (ans < s) ans += k;
    cout << ans << endl;
}
