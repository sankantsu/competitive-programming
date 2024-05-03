#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// (X, Y) -> (x = X+Y, y = X-Y)
// A = (x1, y1), B = (x2, y2)
// dist(A, B) = 1/2(|x2 - x1| + |y2 - y1|)
// Sum_i,j dist(A_i, A_j)
// = 1/2 (Sum_i,j |x_j - x_i| + Sum_i,j |y_j - y_i|)

long dist_sum(const vector<long>& xs) {
    long s = 0;
    long ans = 0;
    for (int i = 0; i < xs.size(); i++) {
        long d = xs[i] - xs[0];
        ans += d*i - s;
        s += d;
    }
    return ans;
}

int main() {
    long n;
    cin >> n;

    vector<long> x(n), y(n);
    rep(i,n) cin >> x[i] >> y[i];

    long ans = 0;
    for (int parity = 0; parity <= 1; parity++) {
        vector<long> xx, yy;
        rep(i,n) {
            if ((x[i] + y[i]) % 2 == parity) {
                xx.push_back(x[i] + y[i]);
                yy.push_back(x[i] - y[i]);
            }
        }

        sort(xx.begin(), xx.end());
        sort(yy.begin(), yy.end());

        ans += (dist_sum(xx) + dist_sum(yy))/2;
    }
    cout << ans << endl;
}
