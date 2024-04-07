#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;
    
    string s;
    cin >> s;

    string t;
    cin >> t;

    vector<vector<long>> pos(26);
    rep(i,s.size()) {
        pos[s[i] - 'a'].push_back(i);
    }

    auto m = s.size();
    auto check = [&](long k) {
        /* bool debug = (k == 344827586207); */
        bool debug = false;
        if (debug) cerr << "k: " << k << endl;
        long p = 0;
        for (auto c : t) {
            const auto& v = pos[c - 'a'];
            auto w = v.size();
            if (w == 0) return false;
            if (debug) {
                cerr << "c: " << c << endl;
                cerr << "v: ";
                for (auto x : v) cerr << x << " ";
                cerr << endl;
            }
            auto d = lower_bound(v.begin(), v.end(), p%m) - v.begin();
            if (debug) cerr << "d: " << d << endl;
            d += k - 1;
            p = p/m*m + d/w*m + v[d%w];
            if (debug) {
                cerr << "p, d: " << p << " " << d << endl;
            }
            if (p >= m*n) return false;
            p++;
        }
        return true;
    };

    long lb = 0;
    long ub = 1L<<60;
    while (ub - lb > 1) {
        long mid = (lb + ub)/2;
        if (check(mid)) {
            lb = mid;
        }
        else {
            ub = mid;
        }
    }
    cout << lb << endl;
}
