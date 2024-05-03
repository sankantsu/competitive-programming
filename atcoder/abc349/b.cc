#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    map<char, int> cnt;
    rep(i, s.size()) {
        cnt[s[i]]++;
    }

    map<int, int> mp;
    for (auto [_, c] : cnt) {
        mp[c]++;
    }

    bool ans = true;
    for (auto [_, c] : mp) {
        if (c == 0 || c == 2) continue;
        else ans = false;
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
