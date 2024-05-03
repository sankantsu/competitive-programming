#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    vector<int> a(9), b(8);

    rep(i,9) cin >> a[i];
    rep(i,8) cin >> b[i];

    int s1 = 0;
    int s2 = 0;
    rep(i,9) s1 += a[i];
    rep(i,8) s2 += b[i];
    /* cerr << s1 << " " << s2 << endl; */

    int ans = max(0, s1 - s2 + 1);
    cout << ans << endl;
}
