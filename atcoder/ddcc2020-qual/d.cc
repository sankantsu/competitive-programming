// D - Digit Sum Replace
// https://atcoder.jp/contests/ddcc2020-qual/tasks/ddcc2020_qual_d
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_m = 200000;
long m;
long d[max_m];
long c[max_m];

int main() {
    cin >> m;
    rep(i,m) cin >> d[i] >> c[i];
    long digit = 0;
    long digit_sum = 0;
    rep(i,m) {
        digit += c[i];
        digit_sum += d[i]*c[i];
    }
    /* cerr << "digit: " << digit << endl; */
    /* cerr << "digit_sum: " << digit_sum << endl; */
    long ans = digit-1;
    while (digit_sum >= 10) {
        long q = digit_sum/10;
        ans += q;
        digit_sum -= 9*q;
    }
    cout << ans << endl;
}
