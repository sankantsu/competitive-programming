#include <iostream>
#include <vector>
#include <algorithm>

/* #include <contest/debug> */

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
long a[200000];

auto factors(int x) {
    vector<int> v;
    for (int i = 1; i*i <= x; i++) {
        if (x%i == 0) {
            v.push_back(i);
            if (x/i != i) v.push_back(x/i);
        }
    }
    return v;
}

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    const int max_n = 200000;
    vector<long> cnt(max_n+1,0);
    rep(i,n) {
        cnt[a[i]]++;
    }

    long res = 0;
    rep(i,max_n+1) {
        auto v = factors(i);
        for (auto x : v) {
            res += cnt[x]*cnt[i/x]*cnt[i];
        }
    }
    cout << res << endl;
}
