#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

int n;
int x[10];
int y[10];

double dist(int i, int j) {
    double dx = x[i]-x[j];
    double dy = y[i]-y[j];
    return sqrt(dx*dx + dy*dy);
}

int factorial(int n) {
    if (n <= 0) return 1;
    return n*factorial(n-1);
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> x[i] >> y[i];
    }

    vector<int> v(n);
    for (int i = 0; i < n; i++) {
        v[i] = i;
    }

    double ans = 0;
    do {
        double d = 0;
        for (int i = 0; i < n-1; i++) {
            d += dist(v[i],v[i+1]);
        }
        ans += d;
    } while (next_permutation(v.begin(),v.end()));
    ans /= factorial(n);

    cout << fixed << setprecision(7) << ans << endl;
}
