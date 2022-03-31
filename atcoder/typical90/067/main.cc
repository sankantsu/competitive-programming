#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

string s;
long k;

constexpr long pow(long a, long b) {
    return (b > 0) ? a*pow(a,b-1) : 1;
}

string base8to9(string s) {
    long x = 0;
    for (int i = 0; i < s.size(); i++) {
        int digit = s[i] - '0';
        // cout << "digit: " << digit << endl;
        x += digit*pow(8,s.size()-i-1);
    }
    // cout << "x: " << x << endl;
    string t;
    if (x == 0) {
        t.push_back('0');
    }
    while (x > 0) {
        int digit = x%9;
        if (digit == 8) {
            t.push_back('5');
        }
        else {
            t.push_back('0' + digit);
        }
        x /= 9;
    }
    reverse(t.begin(),t.end());
    return t;
}

int main() {
    cin >> s >> k;
    for (int i = 0; i < k; i++) {
        s = base8to9(move(s));
    }
    cout << s << endl;
}
