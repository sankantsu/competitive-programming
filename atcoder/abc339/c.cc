#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    vector<long> sum(n+1);
    rep(i,n) {
        sum[i+1] = sum[i] + a[i];
    }

    long min_ = 1L<<60;
    rep(i,n+1) {
        min_ = std::min(min_, sum[i]);
    }

    long ans = sum[n] - min_;
    cout << ans << endl;
}
