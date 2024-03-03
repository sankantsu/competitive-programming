#include <iostream>
#include <ostream>
#include <vector>
#include <string>
#include <algorithm>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    map<char, size_t> mp;
    rep(i,s.size()) {
        mp[s[i]]++;
    }
    
    using P = pair<size_t, char>;
    vector<P> v;
    for (auto [c, cnt] : mp) {
        v.emplace_back(-cnt, c);
    }
    sort(v.begin(), v.end());

    cout << v[0].second << endl;
}
