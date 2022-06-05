#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n;

int main() {
    cin >> n;
    long res = 0;
    for (long i = 1; i <= n; i++) {
        long l = i;
        for (long j = 2; j*j <= l; j++) {
            long k = j*j;
            while (l%k == 0) l /= k;
        }
        for (long j = 1; ; j++) {
            long k = l*j*j;
            if (k <= n) {
                /* cerr << "i,k: " << i << " " << k << endl; */
                res++;
            }
            else {
                break;
            }
        }
    }
    cout << res << endl;
}
