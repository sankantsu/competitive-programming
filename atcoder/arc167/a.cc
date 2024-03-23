#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;
    /* cerr << "n,m: " << n << " " << m << endl; */

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    sort(a.begin(), a.end());
    rep(i, n - m) {
        long j = 2*(n - m) - 1 - i;
        /* cerr << "i,j: " << i << " " << j << endl; */
        a[j] += a[i];
        a[i] = 0;
    }
    
    long ans = 0;
    rep(i,n) ans += a[i]*a[i];
    cout << ans << endl;
}
