#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr double a = pow(2.0,-1.0/1.5);

double p;

double f(double x) {
    return x + p*pow(a,x);
}

int main() {
    cin >> p;
    double lb = 0;
    double ub = p+1;
    while (ub - lb > 1e-8) {
        double c1 = (2*lb+ub)/3;
        double c2 = (lb+2*ub)/3;
        if (f(c1) > f(c2)) lb = c1;
        else ub = c2;
    }
    cout << fixed << setprecision(10) << f(lb) << endl;
}
