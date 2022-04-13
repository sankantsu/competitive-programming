#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

double a,b;

int main() {
    cin >> a >> b;
    double d = sqrt(a*a + b*b);
    double x = a/d;
    double y = b/d;
    cout << x << " " << y << endl;
}
