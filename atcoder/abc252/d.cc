#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 200000;

long n;
long a[max_n+10];

long cnt[max_n+10];

int main() {
    cin >> n;
    rep(i,n) {
        cin >> a[i];
        cnt[a[i]]++;
    }

    long res = n*(n-1)*(n-2)/6;
    rep(j,max_n+1) {
        if (cnt[j] >= 2) {
            long k = cnt[j]*(cnt[j]-1)/2;
            res -= k*(n-cnt[j]);
        }
        if (cnt[j] >= 3) {
            long k = cnt[j]*(cnt[j]-1)*(cnt[j]-2)/6;
            res -= k;
        }
    }
    cout << res << endl;
}
