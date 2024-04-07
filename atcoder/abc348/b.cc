#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> x(n), y(n);
    rep(i,n) {
        cin >> x[i] >> y[i];
    }

    rep(i,n) {
        long d = -1;
        long k = -1;
        rep(j,n) {
            long dx = x[j] - x[i];
            long dy = y[j] - y[i];
            long r = dx*dx + dy*dy;
            if (r > d) {
                d = r;
                k = j;
            }
        }
        cout << k+1 << endl;
    }
}
