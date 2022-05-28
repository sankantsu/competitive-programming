#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int q;
    cin >> q;

    multiset<int> ms;
    rep(i,q) {
        int t;
        cin >> t;
        if (t == 1) {
            int x;
            cin >> x;
            ms.insert(x);
        }
        else if (t == 2) {
            int x,c;
            cin >> x >> c;
            for (auto it = ms.find(x); it != ms.end() && c > 0; it = ms.find(x), c--) {
                ms.erase(it);
            }
        }
        else if (t == 3) {
            int mn = *ms.begin();
            int mx = *ms.rbegin();
            cout << mx-mn << endl;
        }
    }
}
