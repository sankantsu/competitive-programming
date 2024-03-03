#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long q;
    cin >> q;

    vector<long> a;
    rep(i,q) {
        long t, x;
        cin >> t >> x;

        if (t == 1) {
            a.push_back(x);
        }
        else {
            cout << a[a.size() - x] << endl;
        }
    }
}
