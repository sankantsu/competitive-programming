// B - Emblem
// https://atcoder.jp/contests/s8pc-5/tasks/s8pc_5_b
#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
double x[200];
double y[200];
double rd[200];

bool cross(int i, int j, double r) {
    double ri = r;
    if (i < n) {
        ri = rd[i];
    }
    double u = (ri+r)*(ri+r);
    double d = (x[i]-x[j])*(x[i]-x[j]) + (y[i]-y[j])*(y[i]-y[j]);
    return u > d;
}

int main() {
    cin >> n >> m;
    rep(i,n) cin >> x[i] >> y[i] >> rd[i];
    rep(i,m) cin >> x[n+i] >> y[n+i];

    const int numrep = 100;
    double lb = 0;
    double ub = 400;
    rep(i,n) {
        ub = min(ub,rd[i]);
    }
    if (m != 0) {
        rep(k,numrep) {
            double c = (lb+ub)/2;
            rep(j,m) {
                rep(i,n+j) {
                    if (cross(i,n+j,c)) {
                        ub = c;
                        goto ENDLOOP;
                    }
                }
            }
            lb = c;
            ENDLOOP:
            ;
        }
    }
    cout << fixed << setprecision(7);
    cout << ub << endl;
}
