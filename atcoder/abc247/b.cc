#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
string s[200];
string t[200];

map<string,int> mp;

int main() {
    cin >> n;
    rep(i,n) {
        cin >> s[i] >> t[i];
        mp[s[i]]++;
        mp[t[i]]++;
    }
    bool ans = true;
    rep(i,n) {
        if (s[i] == t[i] && mp[s[i]] >= 3) {
            ans = false;
            break;
        }
        else if (s[i] != t[i] && mp[s[i]] >= 2 && mp[t[i]] >= 2) {
            ans = false;
            break;
        }
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}

