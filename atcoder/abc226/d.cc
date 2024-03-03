#include <iostream>
#include <vector>
#include <numeric>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> x(n), y(n);
    rep(i,n) {
        cin >> x[i] >> y[i];
    }

    using P = pair<long, long>;
    set<P> s;
    rep(i,n) {
        rep(j,n) {
            if (i == j) {
                continue;
            }
            long d = gcd(x[j] - x[i], y[j] - y[i]);
            s.emplace((x[j] - x[i])/d, (y[j] - y[i])/d);
        }
    }
    cout << s.size() << endl;
}
