#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    bool ans = true;
    if (s[0] != '<' || s[s.size()-1] != '>') ans = false;
    for (long i = 1; i <= s.size() - 2; i++) if (s[i] != '=') ans = false;

    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
