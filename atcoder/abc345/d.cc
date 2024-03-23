#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

long n, h, w;

using Rect = pair<long, long>;
vector<Rect> rs;
vector<long> area_sum{0};

using Pos = tuple<long, long, long, long>;  // x1, y1, x2, y2
bool dfs(long s, long k, vector<Pos>& pos) {
    if (k == n) {
        return s == h*w;
    }
    bool ans = false;
    auto [a,b] = rs[k];
    auto put = [k, &s, &pos, &ans](long a, long b) {
        rep(i,h-a+1) rep(j,w-b+1) {
            long check = true;
            for (auto [x1,y1,x2,y2] : pos) {
                long x = max(0L, min(x2, i+a) - max(x1, i));
                long y = max(0L, min(y2, j+b) - max(y1, j));
                if (x*y > 0) {
                    check = false;
                    break;
                }
            }
            if (!check) continue;
            pos.emplace_back(i, j, i+a, j+b);
            s += a*b;
            if (s <= h*w && h*w <= s + area_sum[n] - area_sum[k]) {
                ans = ans || dfs(s, k+1, pos);
            }
            pos.pop_back();
            s -= a*b;
            if (ans) return;
        }
    };
    put(a,b);
    if (a != b) {
        put(b,a);  // turn
    }
    if (s <= h*w && h*w <= s + area_sum[n] - area_sum[k]) {
        ans = ans || dfs(s, k+1, pos);  // no use
    }
    return ans;
}

int main() {
    cin >> n >> h >> w;

    rep(i,n) {
        long a, b;
        cin >> a >> b;
        rs.emplace_back(a, b);
    }
    sort(rs.begin(), rs.end(), [](auto lhs, auto rhs){ return lhs.first*lhs.second > rhs.first*rhs.second; });

    long s = 0;
    rep(i,n) {
        auto [a,b] = rs[i];
        s += a*b;
        area_sum.push_back(s);
    }

    bool ans = true;
    if (s < h*w) ans = false;

    vector<Pos> pos;
    ans = ans && dfs(0, 0, pos);

    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
