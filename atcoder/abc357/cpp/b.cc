#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <cctype>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    int small = 0;
    int cap = 0;
    rep(i,s.size()) {
        if (islower(s[i])) {
            small++;
        } else {
            cap++;
        }
    }
    if (cap > small) {
        rep(i,s.size()) {
            s[i] = toupper(s[i]);
        }
    } else {
        rep(i,s.size()) {
            s[i] = tolower(s[i]);
        }
    }
    cout << s << endl;
}
