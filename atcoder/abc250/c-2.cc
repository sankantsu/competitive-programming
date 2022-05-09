#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 200000;

long n,q;
long x[max_n+10];

long val[max_n+10];
long pos[max_n+10];

int main() {
    cin >> n >> q;
    rep(i,q) cin >> x[i];

    for (int i = 1; i <= n; i++) val[i] = i;
    for (int i = 1; i <= n; i++) pos[i] = i;

    rep(i,q) {
        int r = pos[x[i]];
        int s = (r == n) ? r-1 : r+1;
        int a = val[r];
        int b = val[s];
        val[r] = b;
        val[s] = a;
        pos[a] = s;
        pos[b] = r;
    }

    for (int i = 1; i <= n; i++) {
        cout << val[i];
        cout << ((i == n) ? "\n" : " ");
    }
}
