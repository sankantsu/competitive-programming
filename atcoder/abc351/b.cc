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

    vector<string> a(n);
    vector<string> b(n);

    rep(i,n) cin >> a[i];
    rep(i,n) cin >> b[i];

    int x, y;
    rep(i,n) rep(j,n) {
        if (a[i][j] != b[i][j]) {
            x = i;
            y = j;
            break;
        }
    }
    cout << x+1 << " " << y+1 << endl;
}
