// Semi Common Multiple
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
long a[200000];
long b[200000];

long gcd(long a, long b) {
    if (b == 0) return a;
    return gcd(b,a%b);
}

long lcm(long a, long b) {
    return a*b/gcd(a,b);
}

int main() {
    cin >> n >> m;
    rep(i,n) cin >> a[i];
    rep(i,n) b[i] = a[i]/2;
    long l = b[0];
    rep(i,n) {
        l = lcm(l,b[i]);
    }
    bool check = true;
    rep(i,n) {
        if ((l/b[i])%2 == 0) {
            check = false;
            break;
        }
    }
    if (check) {
        long res = (m/l + 1)/2;
        cout << res << endl;
    }
    else {
        cout << 0 << endl;
    }
}
