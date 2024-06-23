#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    vector<int> h(n);
    rep(i,n) cin >> h[i];

    int cnt = 0;
    rep(i,n) {
        if (h[i] <= m) {
            cnt++;
            m -= h[i];
        } else {
            break;
        }
    }
    cout << cnt << endl;
}
