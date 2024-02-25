#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long exclude_square(long x) {
    long res = 1;
    for (long p = 2; p*p <= x; p++) {
        if (x%p != 0) continue;

        long cnt = 0;
        while (x%p == 0) {
            cnt++;
            x /= p;
        }
        if (cnt%2 == 1) {
            res *= p;
        }
    }
    if (x != 1) {
        res *= x;
    }
    return res;
}

int main() {
    long n;
    cin >> n;

    vector<long> A(n);
    rep(i,n) {
        cin >> A[i];
    }

    long zeros = 0;
    using count_t = long;
    map<long, count_t> m;
    rep(i,n) {
        if (A[i] == 0) {
            zeros++;
        }
        else {
            long x = exclude_square(A[i]);
            m[x]++;
        }
    }

    long ans = 0;
    ans += (zeros * (zeros - 1)) / 2;
    ans += zeros * (n - zeros);
    /* cerr << "ans: " << ans << endl; */
    for (auto [x,cnt] : m) {
        /* cerr << "x, cnt: " << x << ", " << cnt << endl; */
        ans += (cnt * (cnt - 1)) / 2;
    }
    cout << ans << endl;
}
