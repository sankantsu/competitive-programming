#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    vector<long> v;
    rep(i,3) {
        long x;
        cin >> x;
        v.push_back(x);
    }
    sort(v.begin(),v.end());
    if (v[0] >= v[2]-v[1]) {
        long res = 0;
        {
            long d = v[2]-v[1];
            v[0] -= d;
            v[2] -= d;
            res += d;
        }
        {
            long d = v[1]-v[0];
            v[1] -= d;
            v[2] -= d;
            res += d;
        }
        assert(v[0] == v[1] && v[1] == v[2]);
        res += v[0];
        cout << res << endl;
    }
    else {
        cout << -1 << endl;
    }
}
