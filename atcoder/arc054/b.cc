#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr double a = pow(2.0,1.0/1.5);
constexpr double b = log(a);

double p;

double f(double x) {
    double y = 1.0 - p*b*pow(a,-x);
    /* cout << "f: " << x << " " << y << endl; */
    return y;
}

double g(double x) {
    return x + p/(pow(a,x));
}

int main() {
    cin >> p;
    double lb = 0;
    double ub = p+1;
    while (ub - lb > 1e-9) {
        double c = (lb+ub)/2;
        if (f(c) < 0) lb = c;
        else ub = c;
    }
    cout << fixed << setprecision(10) << g(lb) << endl;
}
