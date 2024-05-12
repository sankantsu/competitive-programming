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

    vector<string> s(n);
    rep(i,n) cin >> s[i];

    sort(s.begin(), s.end());
    
    long ans = 0;
    rep(i,n) {
        vector<long> v;
        rep(j,s[i].size()) {
            string t = s[i].substr(0, j + 1);
            /* cerr << t << endl; */
            t.push_back('z' + 1);
            auto it = upper_bound(s.begin() + i + 1, s.end(), t);
            long d = it - (s.begin() + i + 1);
            /* cerr << "i,j,d: " << i << " " << j << " " << d << endl; */
            v.push_back(d);
        }
        rep(i, v.size()-1) {
            v[i] -= v[i + 1];
        }
        rep(i, v.size()) {
            ans += (i + 1) * v[i];
        }
    }
    cout << ans << endl;
}
