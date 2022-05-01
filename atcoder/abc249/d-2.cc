#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 200000;
const long max_m = 200000;

long n;
long a[max_n+1];

long cnt[max_m+1];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];
    rep(i,n) cnt[a[i]]++;

    long res = 0;
    for (long b = 1; b <= max_m; b++) {
        for (long c = 1; c <= max_m/b; c++) {
            long a = b*c;
            res += cnt[a]*cnt[b]*cnt[c];
        }
    }
    cout << res << endl;
}
