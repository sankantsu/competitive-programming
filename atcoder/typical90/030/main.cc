// O(n sqrt(n)) TLE

#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

bool check(long x, long k) {
    long cnt = 0;
    for (long p = 2; p*p <= x; p++) {
        if (cnt >= k) {
            return true;
        }
        if (x%p != 0) continue;
        while(x%p == 0) {
            x /= p;
        }
        cnt++;
    }
    if (x != 1) cnt++;
    return cnt >= k;
}

long n,k;

int main() {
    cin >> n >> k;
    long cnt = 0;
    for (long i = 2; i <= n; i++) {
        if (check(i,k)) {
            cnt++;
        }
    }
    cout << cnt << endl;
}
