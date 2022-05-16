#include <iostream>
#include <vector>
#include <algorithm>
#include <iomanip>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 2000;

int n;
long x[max_n+10];
long y[max_n+10];

const double pi = 4*atan(1);

double degree(int i, int j) {
    auto dx = x[j] - x[i];
    auto dy = y[j] - y[i];
    auto d = sqrt(dx*dx + dy*dy);
    auto deg = acos(dx/d);
    if (dy < 0) {
        deg = 2*pi - deg;
    }
    return deg;
}

double center(int i) {
    vector<double> deg;
    rep(j,n) {
        if (j == i) continue;
        deg.push_back(degree(i,j));
    }
    sort(deg.begin(),deg.end());
    double res = 0;
    for (auto d : deg) {
        if (d > pi) break;
        auto it1 = lower_bound(deg.begin(),deg.end(),d+pi);
        auto it2 = prev(it1);
        double r1 = *it1 - d;
        double r2 = *it2 - d;
        if (r1 > pi) r1 = 2*pi - r1;
        if (r2 > pi) r2 = 2*pi - r2;
        res = max(res,r1);
        res = max(res,r2);
    }
    res = res*180/pi;
    return res;
}

int main() {
    cin >> n;
    rep(i,n) cin >> x[i] >> y[i];

    double res = 0;
    rep(i,n) {
        double r = center(i);
        res = max(res,r);
    }
    cout << fixed << setprecision(8);
    cout << res << endl;
}
