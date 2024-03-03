#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

bool is_palindrome(const string& s) {
    bool check = true;
    rep(i, s.size()) {
        check = check & (s[i] == s[s.size() - 1 - i]);
    }
    return check;
}

int main() {
    long n;
    cin >> n;

    long ans = -1;
    rep(i, 10000001) {
        long k = i*i*i;
        if (k > n) break;
        string s = to_string(k);
        if (is_palindrome(s)) {
            ans = k;
        }
    }
    cout << ans << endl;
}
