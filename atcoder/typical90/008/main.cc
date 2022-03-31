#include <iostream>
#include <string>
#include <atcoder/modint>

using namespace std;
using namespace atcoder;

using mint = modint1000000007;

int n;
string s;
string t;

mint dp[128];

int main() {
    const string str("atcoder");
    cin >> n;
    cin >> s;
    for (int i = 0; i < n; i++) {
        auto pos = str.find(s[i]);
        if (pos != string::npos) {
            if (pos == 0) {
                dp[pos]++;
            }
            else {
                dp[pos] += dp[pos-1];
            }
        }
    }
    cout << dp[str.size()-1].val() << endl;
}
