#include <iostream>
#include <string>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int to_int(char c) {
    return c - 'A';
}

int main() {
    string s, t;
    cin >> s;
    cin >> t;

    int x = abs(to_int(s[0]) - to_int(s[1]));
    int y = abs(to_int(t[0]) - to_int(t[1]));
    x = max(x, 5 - x);
    y = max(y, 5 - y);
    if (x == y) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
