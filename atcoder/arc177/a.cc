#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    vector<long> a(6);
    rep(i,6) cin >> a[i];

    long b[6] = {1, 5, 10, 50, 100, 500};

    int n;
    cin >> n;

    vector<long> x(n);
    rep(i,n) cin >> x[i];

    long ans = true;
    for (int i = 0; i < n; i++) {
        long rest = x[i];
        for (int j = 5; j >= 0; j--) {
            if (rest < b[j]) continue;
            long cnt = min(a[j], rest/b[j]);
            rest -= b[j]*cnt;
            a[j] -= cnt;
        }
        if (rest != 0) {
            ans = false;
            break;
        }
    }
    if (ans) {
        cout << "Yes" << endl;
    } else {
        cout << "No" << endl;
    }
}
