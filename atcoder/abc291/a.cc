#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    long k;
    rep(i, s.size()) {
        if (isupper(s[i])) {
            k = i;
        }
    }
    cout << k + 1 << endl;
}
