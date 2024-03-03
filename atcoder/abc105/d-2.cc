#include <iostream>
#include <vector>
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

    long ans = 0;
    long sum = 0;
    map<long, long> cnt;
    cnt[0] = 1;
    rep(i,n) {
        sum = (sum + a[i]) % m;
        ans += cnt[sum];
        cnt[sum]++;
    }
    cout << ans << endl;
}
