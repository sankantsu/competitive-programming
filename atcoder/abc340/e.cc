#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n,m;
    cin >> n >> m;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    vector<long> b(m);
    rep(i,m) {
        cin >> b[i];
    }

    rep(i,m) {
        long x = a[b[i]];
    }
}
