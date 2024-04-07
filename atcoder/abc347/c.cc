#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <random>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

bool solve_jury(long n, long a, long b, const vector<long>& d) {
    long c = a + b;

    vector<long> r(n);
    rep(i,n) r[i] = d[i] % c;
    sort(r.begin(), r.end());
    r.push_back(r[0]);

    long ans = true;
    rep(i,n) {
        if (r[i] != r[0]) ans = false;
    }
    rep(i,n) {
        if ((r[i+1] - r[i] + c) % c > b) {
            ans = true;
        }
    }
    return ans;
}

bool solve(long n, long a, long b, const vector<long>& d) {
    long c = a + b;

    vector<long> r(n);
    rep(i,n) r[i] = d[i] % c;

    // 0 番目の曜日の日付としてありうる範囲
    long lb = 0;
    long ub = 2*(a+b);
    rep(i,n) {
        long x = (r[i] - a + 1 + c) % c;
        if (ub <= x) x -= c;
        else if (x + a <= lb) x += c;
        /* cerr << "lb,ub: " << lb << " " << ub << endl; */
        /* cerr << "x: " << x << endl; */
        lb = max(lb, x);
        ub = min(ub, x+a);
    }
    /* cerr << "lb,ub: " << lb << " " << ub << endl; */
    if (ub <= lb) {
        return false;
    }
    else {
        return true;
    }
}

auto gen_random() {
    static mt19937 mt;
    long n = 3;
    long a = 2;
    long b = 2;
    long c = a + b;
    vector<long> d;
    rep(i,n) {
        long r = (mt() % c) + 1;
        d.push_back(r);
    }
    return make_tuple(n, a, b, d);
}

void test() {
    const int n_test = 100000;
    for (int i = 0; i < n_test; i++) {
        auto [n, a, b, d] = gen_random();
        bool exp = solve_jury(n, a, b, d);
        bool act = solve(n, a, b, d);
        if (exp != act) {
            cerr << "Wrong answer!" << endl;
            cerr << "Expected: " << boolalpha << exp << endl;
            cerr << "Actual: " << boolalpha << act << endl;
            cout << n << " " << a << " " << b << endl;
            rep(i,n) cout << d[i] << " "; cout << endl;
            exit(1);
        }
    }
    exit(0);
}

int main() {
    /* test(); */

    long n, a, b;
    cin >> n >> a >> b;

    long c = a + b;

    vector<long> d(n);
    rep(i,n) cin >> d[i];

    bool ans = solve(n, a, b, d);
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
