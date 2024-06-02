#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, l, r;
    cin >> n >> l >> r;
    l--;

    vector<int> a(n);
    rep(i,n) a[i] = i + 1;

    reverse(a.begin() + l, a.begin() + r);
    rep(i,n) {
        cout << a[i] << " ";
    }
    cout << endl;
}
