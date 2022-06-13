#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long x,a,d,n;

int main() {
    cin >> x >> a >> d >> n;

    long res;

    if (d == 0) {
        res = abs(x-a);
    }
    else {
        long y = (x-a)/d;
        if (y < 0) {
            res = abs(x-a);
        }
        else if (y >= n-1) {
            res = abs(x - (a+d*(n-1)));
        }
        else {
            long r1 = abs(x - (a+d*y));
            long r2 = abs(x - (a+d*(y+1)));
            res = min(r1,r2);
        }
    }
    cout << res << endl;
}
