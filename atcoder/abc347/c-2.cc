#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, a, b;
    cin >> n >> a >> b;

    long c = a + b;

    vector<long> d(n);
    rep(i,n) cin >> d[i];

    vector<long> r(n);
    rep(i,n) r[i] = d[i] % c;
    sort(r.begin(), r.end());
    r.push_back(r[0]);

    long ans = true;
    rep(i,n) {
        if (r[i] != r[0]) ans = false;
    }
    rep(i,n) {
        if ((r[i+1] - r[i] + c) % c > b) {
            ans = true;
        }
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
