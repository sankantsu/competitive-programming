#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    string s, t;
    cin >> s;
    cin >> t;

    if (t[2] == 'X') {
        t = t.substr(0,2);
    }

    int i = 0;
    int j = 0;
    while (i < s.size() && j < t.size()) {
        /* cerr << "i,j: " << i << " " << j << endl; */
        if (s[i] == tolower(t[j])) {
            j++;
        }
        i++;
    }
    
    bool ans = (j == t.size());
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
