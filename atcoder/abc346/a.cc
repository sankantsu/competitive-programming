#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    rep(i,n-1) {
        cout << a[i]*a[i+1] << " ";
    }
    cout << endl;
}
