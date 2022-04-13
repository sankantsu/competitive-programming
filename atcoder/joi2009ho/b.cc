// B - ピザ
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long d,n,m;
long shop[200000];
long customer[20000];

int main() {
    cin >> d >> n >> m;
    rep(i,n-1) cin >> shop[i+1];
    shop[n] = d;
    rep(i,m) cin >> customer[i];

    sort(shop,shop+n+1);
    long res = 0;
    rep(i,m) {
        auto it = lower_bound(shop,shop+n+1,customer[i]);
        if (it == shop) {
            // res += 0;
        }
        else {
            res += min(*it - customer[i], customer[i] - *(it-1));
        }
    }
    cout << res << endl;
}
