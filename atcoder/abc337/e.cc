#include <iostream>
#include <vector>
#include <string>
#include <bitset>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    long nn = 2;
    long m = 1;
    while (nn < n) {
        nn *= 2;
        m += 1;
    }

    vector<bitset<128>> bs;
    long a = 2;
    rep(i,m) {
        bitset<128> b;
        rep(i,n) {
            if (i % a < a/2) {
                b.set(i);
            }
        }
        bs.push_back(b);
        a *= 2;
    }

    cout << m << endl;
    rep(i,m) {
        cout << bs[i].count() << " ";
        rep(j,n) {
            if (bs[i].test(j)) {
                cout << j + 1 << " ";
            }
        }
        cout << endl;
    }

    string s;
    cin >> s;

    bitset<128> cand;
    cand.set();
    rep(i, s.size()) {
        if (s[i] == '0') {
            cand &= ~bs[i];
        }
        else {
            cand &= bs[i];
        }
    }
    rep(i,n) {
        if (cand.test(i)) {
            cout << i + 1 << endl;
        }
    }
}
