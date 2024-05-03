#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <random>
#include <cstdlib>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// e.g
// n = 9, m = 6, k = 2
//     2^n = 1 000 000 000
//     2^m = 0 001 000 000
//     2^k = 0 000 000 100
// 2^(m-k) = 0 000 111 100
//
// 1 000 000 000
// 0 111 100 000
//
// 0 000 100 000

int digits[4] = {2, 4, 8, 6};

int least_digit(long x) {
    if (x == 0) {
        return 0;
    }
    else {
        x = (x-1)%4;
        return digits[x];
    }
}

int solve(long n, long m, long k) {
    long x;
    if (m - k == 1 && k <= n) {
        x = 0;
    }
    else if (n < m) {
        x = n;
    }
    else {
        long r = (n - k) % (m - k);
        x = r + k;
    }
    return least_digit(x);
}

int solve_jury(long n, long m, long k) {
    long N = 1L<<n;
    long M = 1L<<m;
    long K = 1L<<k;
    long r = N % (M - K);
    return r%10;
}

static mt19937 mt;
auto gen_random() {
    const int mx = 18;
    long n = 1 + mt()%mx;
    long m = 2 + mt()%(mx-1);
    long k = 1 + mt()%(m-1);
    return make_tuple(n, m, k);
}

void test() {
    int iter = 0;
    const int max_iter = 100000;
    while (iter++ < max_iter) {
        auto [n, m, k] = gen_random();
        int exp = solve_jury(n, m, k);
        int act = solve(n, m, k);
        if (exp != act) {
            cerr << "Expected: " << exp << endl;
            cerr << "Actual: " << act << endl;
            cout << n << " " << m << " " << k << endl;
            exit(1);
        }
    }
    cerr << "Passed all tests!" << endl;
    exit(0);
}

int main() {
    /* test(); */

    int q;
    cin >> q;

    rep(i,q) {
        long n, m, k;
        cin >> n >> m >> k;
        int ans = solve(n, m, k);
        cout << ans << endl;
    }
}
