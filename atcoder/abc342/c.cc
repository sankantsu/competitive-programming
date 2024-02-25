#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    string s;
    cin >> n;
    cin >> s;

    long q;
    cin >> q;

    char repl[256];
    for (int c = 'a'; c <= 'z'; c++) {
        repl[c] = c;
    }
    rep(i,q) {
        char c, d;
        cin >> c >> d;
        for (int x = 'a'; x <= 'z'; x++) {
            if (repl[x] == c) {
                repl[x] = d;
            }
        }
    }

    for (auto& x : s) {
        x = repl[x];
    }
    cout << s << endl;
}
