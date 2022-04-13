#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

constexpr long inf = 5001*200000;

long a,b,c,x,y;

int main() {
    cin >> a >> b >> c >> x >> y;
    long res = inf;
    for (long i = 0; i <= max(2*x,2*y); i++) {
        long j = max(0L,x - i/2);
        long k = max(0L,y - i/2);
        long cost = a*j + b*k + c*i;
        /* cout << j << " " << k << " " << i << " " << cost << endl; */
        res = min(res,cost);
    }
    cout << res << endl;
}
