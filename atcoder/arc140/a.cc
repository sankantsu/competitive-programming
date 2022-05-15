// A - Right String
// https://atcoder.jp/contests/arc140/tasks/arc140_a

#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
string s;
string t;

auto factors(int n) {
    vector<int> v;
    for (int i = 1; i*i <= n; i++) {
        if (n%i == 0) {
            v.push_back(i);
            if (n/i != i) v.push_back(n/i);
        }
    }
    return v;
}

int mp[128];

// 周期xに編集可能かどうか
bool check(int x) {
    bool ans = false;
    int m = n/x; // 文字列の繰り返し回数
    int res = 0;
    rep(j,x) {
        // 繰り返し周期内にある位置ごとに文字を一致させるためにかかるコストを調べる
        fill(mp,mp+128,0);
        int max_cnt = -1;
        rep(l,m) {
            int idx = j+l*x;
            int c = ++mp[t[idx]];
            max_cnt = max(max_cnt,c);
        }
        res += m - max_cnt;
    }
    if (res <= k) {
        ans = true;
    }
    return ans;
}

int main() {
    cin >> n >> k;
    cin >> s;
    t = s + s;

    auto fs = factors(n);
    sort(fs.begin(),fs.end());
    int res = n;
    for (auto x : fs) {
        if (check(x)) {
            res = x;
            break;
        }
    }
    cout << res << endl;
}
