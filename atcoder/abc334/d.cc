#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, q;
    cin >> n >> q;

    vector<long> r(n);
    rep(i,n) cin >> r[i];
    sort(r.begin(), r.end());

    vector<long> sum(n+1);
    rep(i,n) sum[i+1] = sum[i] + r[i];
    /* rep(i,n) cerr << sum[i+1] << " "; cerr << endl; */

    rep(_,q) {
        long x;
        cin >> x;

        auto it = upper_bound(sum.begin(), sum.end(), x);
        cout << distance(sum.begin(), it) - 1 << endl;
    }
}
