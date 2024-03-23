#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> a(k);
    rep(i,k) cin >> a[i];

    vector<long> diff(k-1);
    rep(i,k-1) diff[i] = a[i+1] - a[i];

    long ans = 0;
    if (k%2 == 0) {
        rep(i,k/2) {
            ans += diff[2*i];
        }
    }
    else {
        ans = 1L<<60;
        vector<long> s1(k/2+1);
        vector<long> s2(k/2+1);
        rep(i,k/2) {
            s1[i+1] = s1[i] + diff[2*i];
            s2[i+1] = s2[i] + diff[k-2-2*i];
        }
        rep(i,k/2 + 1) ans = min(ans, s1[i] + s2[k/2 - i]);
        /* rep(i,k/2 + 1) cerr << s1[i] << " " << s2[k/2-i] << endl; */
    }
    cout << ans << endl;
}
