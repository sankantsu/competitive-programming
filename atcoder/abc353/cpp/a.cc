#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<int> h(n);
    rep(i,n) cin >> h[i];

    int ans = -2;
    int x = h[0];
    for (int i = 1; i < n; i++) {
        if (h[i] > x) {
            ans = i;
            break;
        }
    }
    cout << ans + 1 << endl;
}
