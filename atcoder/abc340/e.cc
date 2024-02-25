#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

// Range sum query
struct SegTree {
    using size_t = long; 
    using value_t = long;
    SegTree(size_t n) {
        _n = 1;
        while (_n < n) {
            _n *= 2;
        }
        _dat.resize(2*_n-1);
        _lazy.resize(2*_n-1);
    }
    void update(size_t i, value_t v) {
        i += _n - 1;
        _dat[i] = v;
        while (i > 0) {
            i = (i - 1)/2;
            _dat[i] = _dat[2*i+1] + _dat[2*i+2];
        }
    }
    void update_range(size_t i, size_t j, value_t v) {
        /* cerr << "(update_range) i, j, v: " << i << ", " << j << ", " << v << endl; */
        _update_range(i, j, v, 0, 0, _n);
    }
    value_t sum(size_t i, size_t j) {
        return _sum(i, j, 0, 0, _n);
    }
    void debug_print() {
        size_t i = 0;
        size_t m = 1;
        cerr << "(debug_print):" << endl;
        while (i < _n) {
            rep(_,m) {
                cerr << _dat[i++] << " ";
            }
            cerr << endl;
            m *= 2;
        }
    }
    private:
    void _propagate(size_t k, size_t len) {
        /* cerr << "(propagate) k, len: " << k << ", " << len << endl; */
        if (_lazy[k] == _e) return;
        if (k < _n - 1) {
            _lazy[2*k+1] = _lazy[2*k+1] + _lazy[k];
            _lazy[2*k+2] = _lazy[2*k+2] + _lazy[k];
        }
        _dat[k] = _dat[k] + _lazy[k]*len;
        _lazy[k] = _e;
    }
    void _update_range(size_t i, size_t j, value_t v, size_t k, size_t l, size_t r) {
        /* cerr << "(_update_range) i,j,v,k,l,r: " << i << ", " << j << ", " << v << ", " << k << ", " << l << ", " << r << endl; */
        _propagate(k, r - l);
        if (r <= i || j <= l) {
            return;
        }
        else if (i <= l && r <= j) {
            _lazy[k] = v;
            _propagate(k, r - l);
        }
        else {
            size_t mid = (l + r)/2;
            _update_range(i, j, v, 2*k+1, l, mid);
            _update_range(i, j, v, 2*k+2, mid, r);
            _dat[k] = _dat[2*k+1] + _dat[2*k+2];
        }
    }
    value_t _sum(size_t i, size_t j, size_t k, size_t l, size_t r) {
        _propagate(k, r - l);
        value_t res;
        if (r <= i || j <= l) {
            res = _e;
        }
        else if (i <= l && r <= j) {
            res = _dat[k];
        }
        else {
            size_t mid = (l + r)/2;
            value_t v1 = _sum(i, j, 2*k+1, l, mid);
            value_t v2 = _sum(i, j, 2*k+2, mid, r);
            res = v1 + v2;
        }
        return res;
    }
    static constexpr value_t _e = 0;
    size_t _n;
    vector<long> _dat;
    vector<long> _lazy;
};

int main() {
    long n,m;
    cin >> n >> m;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    SegTree seg(n);
    rep(i,n) {
        seg.update(i, a[i]);
    }
    /* seg.debug_print(); */

    rep(k,m) {
        long b;
        cin >> b;
        long v = seg.sum(b, b+1);
        /* cerr << "b,v: " << b << ", " << v << endl; */
        /* seg.debug_print(); */
        seg.update(b, 0);
        long q = v/n;
        long r = v%n;
        long start = b + 1;
        long end = start + r;
        if (end <= n) {
            seg.update_range(0, start, q);
            seg.update_range(start, end, q+1);
            seg.update_range(end, n, q);
        }
        else {
            end = end % n;
            seg.update_range(0, end, q+1);
            seg.update_range(end, start, q);
            seg.update_range(start, n, q+1);
        }
        /* rep(i,n) { */
        /*     long idx = (b+i+1)%n; */
        /*     if (i < r) { */
        /*         long cur = seg.sum(idx, idx+1); */
        /*         seg.update(idx, cur+q+1); */
        /*         cerr << "(update) idx, cur, new: " << idx << ", " << cur << ", " << cur+q+1 << endl; */
        /*     } */
        /*     else { */
        /*         long cur = seg.sum(idx, idx+1); */
        /*         seg.update(idx, cur+q); */
        /*         cerr << "(update) idx, cur, new: " << idx << ", " << cur << ", " << cur+q << endl; */
        /*     } */
        /* } */
    }
    
    rep(i,n) {
        cout << seg.sum(i,i+1) << " ";
    }
    cout << endl;
}
