#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    set<long> used;
    rep(i,n) if (a[i] <= k) used.insert(a[i]);

    long s = 0;
    for (auto x : used) {
        s += x;
        /* cerr << "x: " << x << endl; */
        /* cerr << "s: " << s << endl; */
    }

    long ans = k*(k + 1)/2 - s;
    cout << ans << endl;
}
