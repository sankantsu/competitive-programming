#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    map<string, long> mp;
    rep(i,n) {
        string s;
        cin >> s;
        if (mp[s] == 0) {
            cout << s << endl;
        }
        else {
            cout << s << "(" << mp[s] << ")" << endl;
        }
        mp[s]++;
    }
}
