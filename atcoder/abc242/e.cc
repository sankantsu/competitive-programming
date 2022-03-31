#include <iostream>
#include <atcoder/all>
#include <string>

using namespace std;
using namespace atcoder;

constexpr long mod = 998244353;
using mint = modint998244353;

void test(int n, const string &s) {
    mint ans;
    int m = n/2 + n%2;
    for (int i = 0; i < m; i++) {
        int diff = (int)s[i] - 'A';
        ans += diff * pow_mod(26,m-1-i,mod);
    }
    string t = s;
    for (int i = m; i < n; i++) {
        t[i] = s[n-1-i];
    }
    if (t <= s) {
        ans++;
    }
    cout << ans.val() << endl;
}

int main() {
    int t;
    cin >> t;
    for (int i = 0; i < t; i++) {
        int n;
        string s;
        cin >> n >> s;
        test(n,s);
    }
}
