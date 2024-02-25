#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<long> p(n);
    rep(i,n) {
        cin >> p[i];
    }

    long q;
    cin >> q;

    rep(i,q) {
        long a, b;
        cin >> a >> b;
        for (auto x : p) {
            if (x == a) {
                cout << a << endl;
                break;
            }
            else if (x == b) {
                cout << b << endl;
                break;
            }
        }
    }
}
