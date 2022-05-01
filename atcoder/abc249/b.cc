#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

string s;

int main() {
    cin >> s;
    bool flag_small = false;
    bool flag_big = false;
    bool flag_diff = true;
    set<char> cs;
    for (auto c : s) {
        if ('a' <= c && c <= 'z') flag_small = true;
        if ('A' <= c && c <= 'Z') flag_big = true;
        if (cs.find(c) != cs.end()) {
            flag_diff = false;
            break;
        }
        cs.insert(c);
    }
    if (flag_small && flag_big && flag_diff) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
