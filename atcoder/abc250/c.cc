#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, q;
    cin >> n >> q;

    vector<long> a(n);
    rep(i,n) a[i] = i;
    
    vector<long> pos(n);
    rep(i,n) pos[i] = i;

    rep(_,q) {
        /* rep(i,n) cout << a[i] + 1 << " "; cout << endl; */
        long x;
        cin >> x;
        x--;

        long p = pos[x];
        long q = (p == n-1) ? p - 1 : p + 1;
        long y = a[q];

        swap(a[p], a[q]);
        pos[x] = q;
        pos[y] = p;
    }
    rep(i,n) cout << a[i] + 1 << " "; cout << endl;
}
