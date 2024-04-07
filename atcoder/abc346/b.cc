#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long w, b;
    cin >> w >> b;

    string s = "wbwbwwbwbwbw";
    string t;
    while (t.size() < 200 + s.size()) {
        t += s;
    }

    bool ans = false;
    rep(i, s.size()) {
        long cnt = 0;
        rep(j, w + b) {
            if (t[i+j] == 'w') cnt++;
        }
        if (cnt == w) ans = true;
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
