#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct SegTree {
    using size_t = long;
    using value_t = long;
    SegTree(size_t n) {
        _n = 1;
        while (_n < n) {
            _n *= 2;
        }
        _dat.resize(2*_n-1);
    }
    void update(size_t i, value_t x) {
        i += _n - 1;
        _dat[i] = x;
        while (i > 0) {
            i = (i - 1)/2;
            _dat[i] = max(_dat[i], x);
        }
    }
    value_t query(size_t a, size_t b) {
        a = max(0L, a);
        b = min(_n, b);
        return _query(a, b, 0, 0, _n);
    }
private:
    value_t _query(size_t a, size_t b, size_t k, size_t l, size_t r) {
        if (r <= a || b <= l) {
            return -1;
        }
        else if (a <= l && r <= b) {
            return _dat[k];
        }
        else {
            size_t mid = (l + r)/2;
            value_t v1 = _query(a, b, 2*k+1, l, mid);
            value_t v2 = _query(a, b, 2*k+2, mid, r);
            return max(v1, v2);
        }
    }
    size_t _n;
    vector<value_t> _dat;
};

int main() {
    long n, d;
    cin >> n >> d;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    constexpr long max_d = 1000000;
    SegTree seg(max_d);

    vector<long> dp(n+1);
    long ans = -1;
    rep(i,n) {
        long prev = seg.query(a[i] - d, a[i] + d + 1);
        dp[i+1] = prev + 1;
        ans = max(ans, dp[i+1]);

        long cur = seg.query(a[i], a[i]+1);
        if (dp[i+1] > cur) {
            seg.update(a[i], dp[i+1]);
        }
    }
    cout << ans << endl;
}
