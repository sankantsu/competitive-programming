#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;
    
    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    using P = pair<long, long>;
    auto find_seq = [&](long x, long y, long z, vector<P>& ans) {
        // corner case
        if (x == y && y == z) {
            vector<long> pos;
            rep(i,n) {
                if (a[i] == x) pos.push_back(i);
            }
            rep(k, pos.size() - 2) {
                ans.emplace_back(pos[k], pos[k+2]);
            }
            return;
        }
        vector<long> pos(2, -1);
        rep(i,n) {
            if (a[i] == x) {
                pos[0] = i;
            }
            else if (a[i] == y) {
                pos[1] = pos[0];
            }
            else if (a[i] == z && pos[1] >= 0) {
                ans.emplace_back(pos[1], i);
            }
        }
    };

    vector<P> seq_pos;
    for (long k = 1; k <= 10; k++) for (long d = -10; d <= 10; d++) {
        if (k + 2*d > 10) continue;
        find_seq(k, k + d, k + 2*d, seq_pos);
    }
    sort(seq_pos.begin(), seq_pos.end());

    constexpr long inf = 1L<<60;
    vector<long> r(n, inf);
    rep(i,n) {
        auto it = lower_bound(seq_pos.begin(), seq_pos.end(), P{i, -1});
        if (it == seq_pos.end()) continue;
        r[i] = it->second;
    }
    rep(i,n - 1) {
        if (r[n - 1 - i] < r[n - 2 - i]) {
            r[n - 2 - i] = r[n - 1 - i];
        }
    }

    long ans = 0;
    rep(i,n) {
        if (r[i] == inf) continue;
        ans += (n - r[i]);
    }
    cout << ans << endl;
}
