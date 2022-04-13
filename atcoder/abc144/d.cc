#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr double pi = 4*atan(1);
double a,b,x;

bool spill(double th) {
    double y;
    if (b*tan(th) < a) {
        y = b*b*tan(th)/2;
    }
    else {
        y = a*(b - a/tan(th)) + 0.5*a*a/tan(th);
    }
    return y > x/a;
}

int main() {
    cin >> a >> b >> x;
    double lb = 0;
    double ub = pi/2;
    rep(i,100) {
        double c = (lb+ub)/2;
        if (spill(c)) {
            ub = c;
        }
        else {
            lb = c;
        }
    }
    cout << fixed << setprecision(7) << (90 - lb*180/pi) << endl;
}
