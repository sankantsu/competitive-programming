#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, T;
    cin >> n >> T;

    const long inf = 1L<<60;
    long ans = inf;
    rep(i,n) {
        long c, t;
        cin >> c >> t;
        if (t <= T) {
            ans = min(ans, c);
        }
    }
    if (ans == inf) {
        cout << "TLE" << endl;
    }
    else {
        cout << ans << endl;
    }
}
