#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long solve(long n, long p) {  // n, period
    string s = to_string(n);
    long m = s.size();
    long default_ = stol(string(m-1, '9'));
    if (p >= m || m%p != 0) {
        return default_;
    }
    string t = s.substr(0,p);
    string r{};
    rep(i,m/p) r += t;
    long k = stol(r);
    if (k <= n) {
        return k;
    }
    else {
        long c = stol(t) - 1;
        if (c == 0) return default_;
        string t2 = to_string(c);
        while (t2.size() < p) t2 = string("0") + t2;
        r = "";
        rep(i,m/p) r += t2;
        long j = 0;
        return stol(r.substr(j));
    }
}

int main() {
    long t;
    cin >> t;

    rep(i,t) {
        long n;
        cin >> n;
        long ans = -1;
        for (long p = 1; p <= 18; p++) {
            /* cerr << "p, solve(n,p): " << p << " " << solve(n,p) << endl; */
            ans = max(ans, solve(n, p));
        }
        cout << ans << endl;
    }
}
