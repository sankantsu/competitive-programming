#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    vector<long> a(m);
    rep(i,m) cin >> a[i];

    vector<vector<long>> x(n, vector<long>(m));
    rep(i,n) rep(j,m) cin >> x[i][j];

    vector<long> b(m);
    rep(i,n) rep(j,m) b[j] += x[i][j];

    bool ok = true;
    /*rep(i,m) cerr << "i, b[i]: " << i << " " << b[i] << endl;*/
    rep(i,m) if (b[i] < a[i]) ok = false;

    if (ok) {
        cout << "Yes" << endl;
    } else {
        cout << "No" << endl;
    }
}
