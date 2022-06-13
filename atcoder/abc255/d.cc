#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,q;
long a[300000];
long x[300000];

long sum[300000];

int main() {
    cin >> n >> q;
    rep(i,n) cin >> a[i];
    rep(i,q) cin >> x[i];

    sort(a,a+n);
    rep(i,n) {
        sum[i+1] = sum[i] + a[i];
    }

    rep(i,q) {
        int k = distance(a,lower_bound(a,a+n,x[i]));
        long r1 = k*x[i] - sum[k];
        long r2 = sum[n] - sum[k] - (n-k)*x[i];
        long res = r1 + r2;
        cout << res << endl;
    }
}
