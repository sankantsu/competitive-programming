#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int q;
    cin >> q;

    map<int,int> mp;
    rep(i,q) {
        int t;
        cin >> t;
        if (t == 1) {
            int x;
            cin >> x;
            mp[x]++;
        }
        else if (t == 2) {
            int x,c;
            cin >> x >> c;
            c = min(c,mp[x]);
            mp[x] -= c;
            if (mp[x] == 0) mp.erase(x);
        }
        else if (t == 3) {
            int mn = mp.begin()->first;
            int mx = prev(mp.end())->first;
            cout << mx-mn << endl;
        }
    }
}
