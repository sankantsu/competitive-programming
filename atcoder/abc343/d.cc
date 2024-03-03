#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, t;
    cin >> n >> t;

    vector<long> a(t), b(t);
    rep(i,t) {
        cin >> a[i] >> b[i];
        a[i]--;
    }

    vector<long> points(n);
    map<long, size_t> mp;
    mp[0] = n;
    rep(i,t) {
        /* for (auto [x, cnt] : mp) { */
        /*     cerr << "x,cnt: " << x << " " << cnt << endl; */
        /* } */
        /* cerr << endl; */
        long cur = points[a[i]];
        mp[cur]--;
        if (mp[cur] == 0) {
            mp.erase(cur);
        }

        long next = cur + b[i];
        points[a[i]] = next;
        mp[next]++;
        cout << mp.size() << endl;
    }
}
