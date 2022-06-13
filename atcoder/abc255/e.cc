#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
long s[200000];
long x[200000];

int main() {
    cin >> n >> m;
    rep(i,n-1) cin >> s[i];
    rep(i,m) cin >> x[i];

    map<long,int> cnt_even;
    map<long,int> cnt_odd;

    long a = 0;
    rep(i,n) {
        if (i%2 == 0) {
            cnt_even[a]++;
        }
        else {
            cnt_odd[a]++;
        }
        if (i != n-1) {
            a = s[i]-a;
        }
    }

    auto count = [&](long a) {
        long r = 0;
        rep(k,m) {
            if (cnt_even.find(x[k]-a) != cnt_even.end()) {
                r += cnt_even[x[k]-a];
            }
            if (cnt_odd.find(x[k]+a) != cnt_odd.end()) {
                r += cnt_odd[x[k]+a];
            }
        }
        return r;
    };
    long res = 0;
    for (auto [off,c] : cnt_even) {
        rep(j,m) {
            long a = x[j] - off;
            res = max(res,count(a));
        }
    }
    for (auto [off,c] : cnt_odd) {
        rep(j,m) {
            long a = off - x[j];
            res = max(res,count(a));
        }
    }
    cout << res << endl;
}
