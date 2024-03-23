#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <map>
#include <list>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    list<long> a;
    rep(i,n) {
        long x;
        cin >> x;
        a.push_back(x);
    }

    map<long, list<long>::iterator> mp;
    for (auto it = a.begin(); it != a.end(); it++) {
        mp[*it] = it;
    }

    long q;
    cin >> q;
    rep(i,q) {
        long t;
        cin >> t;
        if (t == 1) {
            long x, y;
            cin >> x >> y;

            auto it = mp[x];
            auto it2 = a.insert(next(it), y);
            mp[y] = it2;
        }
        else if (t == 2) {
            long x;
            cin >> x;

            auto it = mp[x];
            a.erase(it);
            mp.erase(x);
        }
    }

    for (auto x : a) {
        cout << x << " ";
    }
    cout << endl;
}
