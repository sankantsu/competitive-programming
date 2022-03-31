#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

// #include <contest/util.h>

using namespace std;

constexpr double pi = atan(1)*4;

double t;
double l,x,y;
int q;

template <typename Vec>
double dist(Vec x, Vec y) {
    double d = 0;
    for (int i = 0; i < x.size(); i++) {
        d += (y[i]-x[i])*(y[i]-x[i]);
    }
    return sqrt(d);
}

int main() {
    cin >> t;
    cin >> l >> x >> y;
    vector<double> p{x,y,0};

    cin >> q;
    for (int i = 0; i < q; i++) {
        double t_i;
        cin >> t_i;
        double y_i = (-1)*(l/2)*sin(2*pi*t_i/t);
        double z_i = l/2 - (l/2)*cos(2*pi*t_i/t);
        // print_vals("i,y_i,z_i:",i,y_i,z_i);
        vector<double> p_i{0,y_i,z_i};
        vector<double> q_i{0,y_i,0};
        double a = dist(p,p_i);
        double b = dist(p,q_i);
        // print_vals("a,b:",a,b);
        double th = acos(b/a);
        th = th*180/pi;
        cout << fixed << setprecision(7) << th << endl;
    }
}
