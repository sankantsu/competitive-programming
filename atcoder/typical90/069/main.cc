#include <iostream>
#include <atcoder/modint>

// #include <contest/util.h>

using namespace std;
using mint = atcoder::modint1000000007;

long n,k;

mint pow(mint a, long b) {
    // cout << "pow " << a.val() << " " << b << endl;
    mint p = a;
    mint ans = 1;
    while (b > 0) {
        if (b & 0x1) {
            ans *= p;
        }
        // print_vals(b,b&1,p.val(),ans.val());
        p *= p;
        b /= 2;
    }
    return ans;
}

int main() {
    cin >> n >> k;

    if (n == 1) {
        cout << k << endl;
        return 0;
    }

    mint ans = k*(k-1);
    mint p = pow(k-2,n-2);
    // cout << "p: " << p.val() << endl;

    ans *= p;

    cout << ans.val() << endl;
    return 0;
}
