// B - Emblem
// https://atcoder.jp/contests/s8pc-5/tasks/s8pc_5_b
#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
double x[200];
double y[200];
double rd[200];

int main() {
    cin >> n >> m;
    rep(i,n) cin >> x[i] >> y[i] >> rd[i];
    rep(i,m) cin >> x[n+i] >> y[n+i];
    double res = 400;
    rep(i,n) {
        res = min(res,rd[i]);
    }
    for (int i = 0; i < n+m; i++) {
        for (int j = i+1; j < n+m; j++) {
            double dist = sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (i < n && j < n) continue;
            else if (i < n && n <= j) {
                res = min(res,dist-rd[i]);
            }
            else if (n <= i && n <= j) {
                res = min(res,dist/2);
            }
            else {
                throw;
            }
        }
    }
    cout << fixed << setprecision(7);
    cout << res << endl;
}
