#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
int a[1000];
long x[1000];
long y[1000];
long dist[1000];

long distance(int i, int j) {
    long dx = x[i]-x[j];
    long dy = y[i]-y[j];
    return dx*dx + dy*dy;
}

int main() {
    cin >> n >> k;
    rep(i,k) {
        cin >> a[i];
        a[i]--;
    }
    rep(i,n) cin >> x[i] >> y[i];
    rep(i,n) dist[i] = 1L<<60;

    rep(i,n) {
        rep(j,k) {
            dist[i] = min(dist[i],distance(i,a[j]));
        }
    }
    long res = 0;
    rep(i,n) {
        res = max(res,dist[i]);
    }

    double r = sqrt(res);
    cout << setprecision(10) << r << endl;
}
