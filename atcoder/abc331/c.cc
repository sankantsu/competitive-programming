#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<long> a(n);
    rep(i,n) cin >> a[i];

    vector<long> b = a;
    sort(b.begin(), b.end(), [](long x, long y) { return x > y; });
    b.push_back(0);

    long s = 0;
    long cur = b[0] + 1;
    map<long, long> mp;
    for (auto x : b) {
        if (x != cur) {
            mp[x] = s;
            cur = x;
        }
        s += cur;
    }

    rep(i,n) {
        cout << mp[a[i]] << " ";
    }
    cout << endl;
}
