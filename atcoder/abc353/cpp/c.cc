#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    const long mod = 100000000;
    rep(i,n) a[i] = a[i] % mod;
    sort(a.begin(), a.end());

    long s = 0;
    rep(i,n) s += a[i];
    /* cerr << "s: " << s << endl; */

    long cnt = 0;
    /* rep(i,n) { */
    /*     cerr << "i,a: " << i << " " << a[i] << endl; */
    /* } */
    rep(i,n) {
        auto it = std::lower_bound(a.begin()+i+1, a.end(), mod - a[i]);
        long c = a.end() - it;
        /* cerr << "i,c: " << i << " " << c << endl; */
        cnt += c;
    }

    long ans = (n - 1)*s - cnt*mod;
    cout << ans << endl;
}
