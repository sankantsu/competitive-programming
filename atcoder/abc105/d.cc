#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, m;
    cin >> n >> m;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    vector<long> sum(n);
    sum[0] = a[0] % m;
    for (long i = 1; i < n; i++) {
        sum[i] = (sum[i-1] + a[i]) % m;
    }

    map<long, long> mp;  // value -> cnt
    rep(i,n) {
        mp[sum[i]]++;
    }

    long ans = 0;
    ans += mp[0];
    for (auto [v, cnt] : mp) {
        /* cerr << "v,cnt: " << v << " " << cnt << endl; */
        ans += cnt * (cnt - 1) / 2;
    }
    cout << ans << endl;
}
