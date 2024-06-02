#include <iostream>
#include <random>
#include <atcoder/modint.hpp>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

#ifndef DEBUG
#define DEBUG 0
#endif

#ifndef TEST
#define TEST 0
#endif

constexpr int max_digit = DEBUG ? 5 : 61;
mint dp[max_digit + 1][max_digit + 1][2];  // i: ケタ, j: popcnt of x&m, k: exact -> 個数

long solve_jury(long n, long m) {
    mint ans = 0;
    for (long k = 0; k <= n; k++) {
        long cnt = 0;
        rep(i, max_digit) {
            if ((k&m) & (1L<<i)) cnt++;
        }
        ans += cnt;
    }
    return ans.val();
}

long solve(long n, long m) {
    // init
    rep(i, max_digit+1) rep(j, max_digit+1) rep(k, 2) dp[i][j][k] = 0;

    dp[max_digit][0][1] = 1;
    for (int i = max_digit; i > 0; i--) {
        int ni = i - 1;
        long mask = 1L<<ni;
        rep(j, max_digit) {
            rep(digit, 2) {  // digit for ni th bit
                long x = digit<<ni;
                // exact -> exact
                if (x == (n&mask)) {
                    int nj = ((n&m) & mask) ? j + 1 : j;
                    dp[ni][nj][1] += dp[i][j][1];
                }
                // exact -> small
                if (x < (n&mask)) {
                    int nj = j;
                    dp[ni][nj][0] += dp[i][j][1];
                }
                // small -> small
                {
                    int nj = (x&m) ? j + 1 : j;
                    dp[ni][nj][0] += dp[i][j][0];
                }
            }
        }
    }
#if DEBUG == 1
    for (int i = max_digit; i >= 0; i--) {
        rep(j, max_digit) {
            cerr << "i,j,exact,small: " << i << " " << j << " " << dp[i][j][1].val() << " " << dp[i][j][0].val() << endl;
        }
        cerr << endl;
    }
#endif

    mint ans = 0;
    rep(j, max_digit) {
        rep(k, 2) {
            ans += dp[0][j][k] * j;
        }
    }

    return ans.val();
}

auto gen_testcase() {
    static mt19937 mt;
    int ndigit = 4;
    long n = mt() % (1L<<ndigit);
    long m = mt() % (1L<<ndigit);
    return make_pair(n, m);
}

void test() {
    int n_test = 10000;
    rep(i, n_test) {
        auto [n, m] = gen_testcase();
        long expect = solve_jury(n, m);
        long actual = solve(n, m);
        if (expect != actual) {
            cerr << "Wrong anser!" << endl;
            cerr << "Expected: " << expect << endl;
            cerr << "But, got: " << actual << endl;
            cout << n << " " << m << endl;
            exit(1);
        }
    }
    cerr << "All test passed!" << endl;
    exit(0);
}

int main() {
#if TEST == 1
    test();
#endif

    long n, m;
    cin >> n >> m;
    long ans = solve(n, m);

    cout << ans << endl;
}
