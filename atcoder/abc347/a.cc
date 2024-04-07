#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    rep(i,n) {
        if (a[i] % k == 0) {
            cout << a[i]/k << " ";
        }
    }
    cout << endl;
}
