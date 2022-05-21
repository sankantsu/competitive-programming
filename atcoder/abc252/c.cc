#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
string s[200];

auto calc() {
    int cnt[10][10];
    rep(c,10) rep(k,10) cnt[c][k] = 0;
    rep(j,n) {
        rep(k,10) {
            int c = s[j][k] - '0';
            cnt[c][k]++;
        }
    }
    vector<int> v(10);
    rep(c,10) {
        int t = 0;
        rep(k,10) {
            int ct = cnt[c][k];
            t = max(t,10*(ct-1)+(int)k);
        }
        v[c] = t;
    }
    return v;
}

int main() {
    cin >> n;
    rep(i,n) cin >> s[i];

    auto v = calc();

    int res = 1<<29;
    rep(i,10) {
        res = min(res,v[i]);
    }
    cout << res << endl;
}
