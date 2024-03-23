#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    long ans;
    if (a[n-1] - a[n-2] >= 2) {
        ans = true;
    }
    else if ((a[n-1]-n+2) % 2 == 0) {
        ans = true;
    }
    else {
        ans = false;
    }
    if (ans) {
        cout << "Alice" << endl;
    }
    else {
        cout << "Bob" << endl;
    }
}
