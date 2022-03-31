#include <iostream>
#include <algorithm>
#include <cmath>

using namespace std;

int n;
long x[100000];
long y[100000];

long min_distance(int n, long *p) {
    sort(p,p+n);
    long x = p[n/2];
    long res = 0;
    for (int i = 0; i < n; i++) {
        res += abs(x-p[i]);
    }
    return res;
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> x[i] >> y[i];
    }

    long res = 0;
    res += min_distance(n,x);
    res += min_distance(n,y);

    cout << res << endl;
}
