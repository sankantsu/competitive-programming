#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    string s;
    cin >> s;

    using P = pair<int, int>;
    vector<P> v;

    int l = 0;
    while (l < n) {
        int r = l + 1;
        while (r < n and s[l] == s[r]) ++r;
        if (s[l] == '1') {
            v.emplace_back(l, r);
        }
        l = r;
    }
    reverse(v.begin(), v.end());

    string t;
    for (auto [l, r] : v) {
        string u(r, 'A');
        string v(l, 'B');
        t += u + v;
    }
    cout << t.size() << endl;
    cout << t << endl;
}
