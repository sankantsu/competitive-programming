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

    vector<long> a(n+2);
    a[0] = 0; a[n+1] = 1;
    rep(i,n) {
        cin >> a[i+1];
    }

    vector<long> l(n+2);  // 左にいくつ伸ばせるか?
    vector<long> r(n+2);  // 右にいくつ伸ばせるか?
    rep(i,n) {
        l[i+1] = min(l[i]+1, a[i+1]);
        r[n-i] = min(r[n+1-i]+1, a[n-i]);
    }
    /* rep(i,n) { */
    /*     cerr << "i,l,r: " << i+1 << " " << l[i+1] << " " << r[i+1] << endl; */
    /* } */
    long ans = -1;
    rep(i,n) {
        ans = max(ans, min(l[i+1], r[i+1]));
    }
    cout << ans << endl;
}
