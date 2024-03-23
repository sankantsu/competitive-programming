#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long h, w, k;
    cin >> h >> w >> k;

    vector<string> s(h);
    rep(i,h) {
        cin >> s[i];
    }

    constexpr long inf = 1L<<60;
    long ans = inf;
    if (k <= w) {
        rep(i,h) {
            vector<long> x_cnt(w+1);
            vector<long> o_cnt(w+1);
            rep(j,w) {
                x_cnt[j+1] = x_cnt[j] + (s[i][j] == 'x' ? 1 : 0);
                o_cnt[j+1] = o_cnt[j] + (s[i][j] == 'o' ? 1 : 0);
            }
            rep(j, w-k+1) {
                long x = x_cnt[j+k] - x_cnt[j];
                long o = o_cnt[j+k] - o_cnt[j];
                if (x == 0) {
                    ans = min(ans, k - o);
                }
            }
        }
    }
    if (k <= h) {
        rep(j,w) {
            vector<long> x_cnt(h+1);
            vector<long> o_cnt(h+1);
            rep(i,h) {
                x_cnt[i+1] = x_cnt[i] + (s[i][j] == 'x' ? 1 : 0);
                o_cnt[i+1] = o_cnt[i] + (s[i][j] == 'o' ? 1 : 0);
            }
            rep(i, h-k+1) {
                long x = x_cnt[i+k] - x_cnt[i];
                long o = o_cnt[i+k] - o_cnt[i];
                if (x == 0) {
                    ans = min(ans, k - o);
                }
            }
        }
    }
    if (ans == inf) {
        cout << -1 << endl;
    }
    else {
        cout << ans << endl;
    }
}
